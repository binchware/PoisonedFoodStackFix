using HarmonyLib;
using RimWorld;
using System;
using System.Reflection;
using Verse;

namespace PoisonedFoodStackFix
{
    /// <summary>
    /// Hypergeometric distribution.
    /// </summary>
    public static class Hypergeometric
    {
        /// <summary>
        /// Random generation for the Hypergeometric(N, K, n) distribution.<br/>
        /// An urn has N balls, K are green, n are drawn. How many green balls are drawn?<br/>
        /// Valid parameters:<br/>
        /// 0 ≤ N ≤ int.MaxValue<br/>
        /// 0 ≤ K ≤ N<br/>
        /// 0 ≤ n ≤ N<br/>
        /// Warning: validity of parameters is not checked.<br/>
        /// Performance note. Hypergeometric(1M, 500K, 500K) is generated in ≈31 ms on a 2.7 GHz CPU.
        /// </summary>
        /// <param name="N">Number of balls in urn.</param>
        /// <param name="K">Number of green balls in urn.</param>
        /// <param name="n">Number of drawn balls.</param>
        /// <returns>Number of drawn green balls.</returns>
        public static int Generate(int N, int K, int n)
        {
            // Let pmf(k, N, K, n) be probability mass function for Hypergeometric(N, K, n).
            // Using symmetry of pmf, it is possible to optimize the algorythm, aiming for smaller value of drawn balls.

            // pmf(k1, N, K, n) = pmf(n - k1, N, N - K, n)
            // If number of non-green balls is less than number of green ones, swap colors.
            // k1 is number of drawn green balls, the future return value of the function.
            int G = N - K;
            bool swapColors = G < K;
            if (!swapColors) G = K;

            // pmf(k2, N, G, n) = pmf(G - k2, N, G, N - n)
            // If number of left balls is less than number of drawn ones, swap sides, draw the number of balls that would be left.
            // k2 is number of drawn green balls, considering a possible color swap.
            int nn = N - n;
            bool swapSides = nn < n;
            if (!swapSides) nn = n;

            // Number of green balls left in the urn. Extra variable is created, because the value of G should be preserved.
            int g = G;
            // pmf(k, N, g, nn) = pmf(k, N, nn, g)
            // If number of green balls is less than number of drawn ones, swap roles, swapping these numbers.
            // k is number of drawn green balls. It is not affected by the swap.
            if (g < nn) (g, nn) = (nn, g);

            // At this point, 0 ≤ nn ≤ g ≤ (N / 2), and the distribution to generate is Hypergeometric(N, g, nn).

            // k is number of drawn green balls, considering all possible swaps.
            // When values of k2 and k1 will be calculated, they will also be stored here for simplicity.
            int k = 0;
            while (--nn >= 0)
            {
                if (Rand.Chance((float)g / N))
                {
                    --g;
                    ++k;
                }
                --N;
            }

            // Revert symmetry swaps, if any.
            if (swapSides) k = G - k; // k has value of k2 now.
            if (swapColors) k = n - k; // k has value of k1 now.

            return k;
        }

#if DEBUG
        // Test case description.
        private struct TestCase
        {
            // Parameters of distribution.
            public int N, K, n;
            // Number of runs to perform.
            public int runs;
        }
        // Pre-defined test cases.
        private static readonly TestCase[] testCases = new TestCase[]
        {
            new TestCase() { N = 0, K = 0, n = 0, runs = 2 },

            new TestCase() { N = 10, K = 5, n = 5, runs = 1000 },

            // These tests should trigger all 8 permutations of symmetry swaps in Generate().
            new TestCase() { N = 10, K = 2, n = 4, runs = 10_000 },
            new TestCase() { N = 10, K = 2, n = 6, runs = 10_000 },
            new TestCase() { N = 10, K = 8, n = 4, runs = 10_000 },
            new TestCase() { N = 10, K = 8, n = 6, runs = 10_000 },
            new TestCase() { N = 10, K = 4, n = 2, runs = 10_000 },
            new TestCase() { N = 10, K = 4, n = 8, runs = 10_000 },
            new TestCase() { N = 10, K = 6, n = 2, runs = 10_000 },
            new TestCase() { N = 10, K = 6, n = 8, runs = 10_000 },

            new TestCase() { N = 35, K = 1, n = 19, runs = 1000 },
            new TestCase() { N = 44, K = 4, n = 29, runs = 1000 },

            new TestCase() { N = 10_000, K = 9900, n = 9000, runs = 100_000 },
            new TestCase() { N = 1_000_000, K = 500_000, n = 500_000, runs = 500 },
        };

        /// <summary>
        /// Run pre-defined test cases and a number of test cases with random parameters.
        /// </summary>
        public static void Test()
        {
            foreach (TestCase tc in testCases)
                Test1Case(tc);

            TestCase t;
            Random rnd = new Random();
            for (int i = 0; i < 20; ++i)
            {
                t.N = rnd.Next(100 + 1); // 0 <= N <= 100
                t.K = rnd.Next(t.N + 1); // 0 <= K <= N
                t.n = rnd.Next(t.N + 1); // 0 <= n <= N
                t.runs = 10_000;
                Test1Case(t);
            }
        }

        /// <summary>
        /// Run a test case.
        /// </summary>
        /// <param name="tc">Test case.</param>
        private static void Test1Case(TestCase tc)
        {
            // Results. For each possible outcome, the number of its appearances in the generated statistical sample.
            int[] r = new int[tc.n + 1];

            // Generate the sample and measure execution time.
            System.Diagnostics.Stopwatch stopwatch = System.Diagnostics.Stopwatch.StartNew();
            for (int i = 0; i < tc.runs; ++i)
            {
                ++r[Generate(tc.N, tc.K, tc.n)];
            }
            stopwatch.Stop();
            long time = stopwatch.ElapsedMilliseconds;

            // True mean.
            float mean = (float)tc.n * tc.K / tc.N;
            // Sum of observations.
            long sum = 0;
            // Sum of squares of variance about the known mean.
            float sum2 = 0;
            for (int k = 0; k <= tc.n; ++k)
            {
                // Number of observations of outcome k.
                int n = r[k];
                sum += (long)n * k;
                sum2 += n * (k - mean) * (k - mean);
            }

            float sampleMean = (float)sum / tc.runs;
            float meanDelta = sampleMean - mean;
            // Sample variance about the known mean. It is unbiased, because true mean is used.
            float sampleVariance = sum2 / tc.runs;
            float variance = sampleMean * (tc.N - tc.K) / tc.N * (tc.N - tc.n) / (tc.N - 1);
            float varianceDelta = sampleVariance - variance;
            // Are test results good enough? Relative errors are calculated.
            bool good = Math.Abs(meanDelta) / (mean == 0 ? 1f : mean) < 0.05 &&
                Math.Abs(varianceDelta) / (variance == 0 ? 1f : variance) < 0.05;
            // Call either Log.Message or Log.Warning, depending on test case results. Lines with "bad" results will stand out visually.
            (good ? new Action<string>(Log.Message) : new Action<string>(Log.Warning)) (string.Format(
                "{0:#,0}, {1:#,0}, {2:#,0} × {3:#,0}: {4:#,0} ms {5:#,0} ns per, mean {6} Δ {7}, variance {8} Δ {9}",
                tc.N, tc.K, tc.n, tc.runs, time, time * 1_000_000 / tc.runs, sampleMean, meanDelta, sampleVariance, varianceDelta));
        }
#endif
    }

    [StaticConstructorOnStartup]
    public static class HarmonyPatches
    {
        private static readonly Type patchType = typeof(HarmonyPatches);

        static HarmonyPatches()
        {
            Harmony harmony = new Harmony(id: "binchcannon.rimworld.PoisonedFoodStackFix");

            // patch to better distribute poison percent when splitting a stack
            harmony.Patch(AccessTools.Method(
                    typeof(CompFoodPoisonable),
                    nameof(CompFoodPoisonable.PostSplitOff)),
                postfix: new HarmonyMethod(patchType, nameof(SplitFoodPoisonablePostfix)));

            // patch with prefix to record poison data before merging stacks
            // patch with postfix to better distribute poison percent when merging stacks
            harmony.Patch(AccessTools.Method(
                    typeof(CompFoodPoisonable),
                    nameof(CompFoodPoisonable.PreAbsorbStack)),
                prefix: new HarmonyMethod(patchType, nameof(AbsorbStackPoisonablePrefix)),
                postfix: new HarmonyMethod(patchType, nameof(AbsorbStackPoisonablePostfix)));

            #if DEBUG
                Log.Error("I hereby summon thee, Debug log window!");
                Hypergeometric.Test();
            #endif
        }

        /// <summary>
        /// Postfix for CompFoodPoisonable.PostSplitOff() method.<br/>
        /// The method is used to adjust poison data after splitting a stack.<br/>
        /// The postfix corrects the original game behavior, if needed, adjusting data of stacks.<br/>
        /// If invalid values are encountered, the postfix leaves everything as is.
        /// </summary>
        /// <param name="__instance">Component of the base stack. It has the post-split values for count of items and poison-related data.</param>
        /// <param name="___poisonPct">Poison percentage of the base stack.</param>
        /// <param name="piece">New stack after splitting. Count of items and poison-related data are set already.</param>
        public static void SplitFoodPoisonablePostfix(CompFoodPoisonable __instance, ref float ___poisonPct, Thing piece)
        {
            #if DEBUG
                Log.Message("==================== Splitting ====================");
            #endif

            // An oddity of RimWorld code should be delt here with.
            // Sometimes, Thing.SplitOff() method is asked to split a stack, and the new piece is asked to hold the whole stack.
            // Notably, this happens after a pawn picks up a stack.
            // The method simply points out to the original stack as a new piece, not actually creating a new Thing.
            // Then, CompFoodPoisonable.PostSplitOff() is called. It probably shouldn't be, but it is.
            // When this happens, new stack (piece parameter) is the same object as original stack (__instance.parent).
            // Skip in case.
            if (ReferenceEquals(__instance.parent, piece))
            {
                #if DEBUG
                    Log.Message(string.Format("Odd: {0} of {1} pcs at {2}% ({3}) is split to self",
                        __instance.parent.ThingID, __instance.parent.stackCount, ___poisonPct * 100, __instance.cause));
                #endif
                return;
            }

            #if DEBUG
                Log.Message(string.Format("Preliminary: {0} of {1} pcs at {2}% ({3}) and {4} of {5} pcs at {6}% ({7})",
                    __instance.parent.ThingID, __instance.parent.stackCount, ___poisonPct * 100, __instance.cause,
                    piece.ThingID, piece.stackCount, piece.TryGetComp<CompFoodPoisonable>().PoisonPercent * 100, piece.TryGetComp<CompFoodPoisonable>().cause));
            #endif

            // skip if entire base stack is either clean or poisoned
            // - base game behavior should be adequate
            // At the same time, do sanity check.
            if (___poisonPct <= 0 || ___poisonPct >= 1) return;

            // Component of split piece encapsulating information on poisoned items.
            CompFoodPoisonable splitComp = piece.TryGetComp<CompFoodPoisonable>();
            // Sanity check.
            if (splitComp is null) return;

            // verify expectations
            // - base game behavior is to give split stack same poisonPct as base stack
            // - if this isn't the case, then base game behavior has changed or another mod is handling this
            if (___poisonPct != splitComp.PoisonPercent) return;

            // Items left in base stack.
            int baseLeft = __instance.parent.stackCount;

            // skip if either stack is empty
            // - base game behavior should be adequate
            // At the same time, do sanity check.
            if (baseLeft <= 0 || piece.stackCount <= 0) return;

            // Initial number of items in the base stack.
            int baseTotal = baseLeft + piece.stackCount;
            // Initial number of poisoned items in the base stack.
            // Rounding to the nearest whole number is required to correct rounding errors of floating-point calculations.
            // Also, in case the mod was added to an existing save, the involved stack may have e.g. 0.9 or 0.4 poisoned items,
            // and these numbers are converted to realistic whole numbers.
            // This is not totally fair, but can be considered as fixing of problems with poison in food stacks, totally.
            int basePoison = (int)Math.Round(baseTotal * ___poisonPct);

            #if DEBUG
                Log.Message(string.Format("Poisoned: {0}/{1} in total", basePoison, baseTotal));
            #endif

            // Number of poisoned items moved to the split piece.
            // Hypergeometric distribution; sample randomly from base stack without replacement.
            int splitPoison = Hypergeometric.Generate(N: baseTotal, K: basePoison, n: piece.stackCount);
            // Number of poisoned items left in the base stack.
            basePoison -= splitPoison;

            // update split poison props
            float splitPoisonPct = (float)splitPoison / piece.stackCount;
            typeof(CompFoodPoisonable).GetField("poisonPct", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(splitComp, splitPoisonPct);
            if (splitPoison == 0) splitComp.cause = FoodPoisonCause.Unknown;

            // update base poison props
            ___poisonPct = (float)basePoison / baseLeft;
            if (basePoison == 0) __instance.cause = FoodPoisonCause.Unknown;

            #if DEBUG
                Log.Message(string.Format("Split: {0}/{1} pcs at {2}% ({3}) and {4}/{5} pcs at {6}% ({7})",
                    basePoison, baseLeft, ___poisonPct * 100, __instance.cause,
                    splitPoison, piece.stackCount, splitComp.PoisonPercent * 100, splitComp.cause));
            #endif
        }

        public struct PoisonData
        {
            public float poisonPct;
            public FoodPoisonCause cause;
        }

        /// <summary>
        /// Prefix for CompFoodPoisonable.PreAbsorbStack() method.<br/>
        /// The method is used to adjust poison data before merging stacks.<br/>
        /// The prefix stores poison data of the target stack to the parameter __state.
        /// </summary>
        /// <param name="__instance">Component of the target stack.</param>
        /// <param name="___poisonPct">Poison percentage of the target stack.</param>
        /// <param name="__state">Poison data of the target stack.</param>
        public static void AbsorbStackPoisonablePrefix(CompFoodPoisonable __instance, float ___poisonPct, out PoisonData __state)
        {
            #if DEBUG
                Log.Message("==================== Absorbing prefix ====================");
                Log.Message(string.Format("Stack {0} of {1} pcs at {2}% ({3})",
                    __instance.parent.ThingID, __instance.parent.stackCount, ___poisonPct * 100, __instance.cause));
            #endif

            // record original poisonPct and cause
            __state.poisonPct = ___poisonPct;
            __state.cause = __instance.cause;
        }

        /// <summary>
        /// Postfix for CompFoodPoisonable.PreAbsorbStack() method.<br/>
        /// The method is used to adjust poison data before merging stacks.<br/>
        /// The postfix corrects the original game behavior, if needed, adjusting data of both stacks.<br/>
        /// If invalid values are encountered, the postfix leaves everything as is.
        /// </summary>
        /// <param name="__instance">Component of the target stack. It has updated values for percentage and cause, while the <i>parent</i> stack has the original (pre-absorbtion) count of items.</param>
        /// <param name="___poisonPct">Updated poison percentage of the target stack.</param>
        /// <param name="otherStack">Donor stack. It has the original, pre-absorbtion values for count of items and poison-related data.</param>
        /// <param name="count">Count of absorbed items.</param>
        /// <param name="__state">Original, not updated poison data of the target stack, saved by the prefix.</param>
        public static void AbsorbStackPoisonablePostfix(CompFoodPoisonable __instance, ref float ___poisonPct, Thing otherStack, int count, PoisonData __state)
        {
            #if DEBUG
                Log.Message("==================== Absorbing postfix ====================");
            #endif

            // Component of other stack encapsulating information on poisoned items.
            CompFoodPoisonable otherComp = otherStack.TryGetComp<CompFoodPoisonable>();
            // Sanity check.
            if (otherComp is null) return;

            #if DEBUG
                Log.Message(string.Format("Preliminary: {0} of {1}({2}) pcs at {3}% ({4}) after absorbing {5} pcs from {6} of {7} pcs at {8}% ({9})",
                    __instance.parent.ThingID, __instance.parent.stackCount, __instance.parent.stackCount + count, ___poisonPct * 100, __instance.cause,
                    count, otherStack.ThingID, otherStack.stackCount, otherComp.PoisonPercent * 100, otherComp.cause));
            #endif

            // skip if entire other stack is either clean or poisoned
            // - base game behavior should be adequate
            // At the same time, do sanity check.
            float otherPoisonPct = otherComp.PoisonPercent;
            if (otherPoisonPct <= 0 || otherPoisonPct >= 1) return;

            // skip if we're absorbing entire other stack
            // - base game behavior should be adequate
            // Also, do sanity check.
            int otherTotal = otherStack.stackCount;
            if (count >= otherTotal) return;

            // verify expectations
            // - base game behavior is to set poisonPct based on weighted average
            // - if this isn't the case, then base game behavior has changed or another mod is handling this
            int targetTotal = __instance.parent.stackCount;
            float weightedAvg = GenMath.WeightedAverage(__state.poisonPct, targetTotal, otherPoisonPct, count);
            if (___poisonPct != weightedAvg) return;

            // Performance-wise, sanity checks that should never trigger are performed last.
            // Unlike PostSplitOff() case, source and target stacks don't seem to coincide in the normal workflow. So, just a sanity check.
            if (ReferenceEquals(__instance.parent, otherStack)) return;
            if (targetTotal < 0) return;
            // The case (count == 0) is not absolutely insane, but no action is needed anyway.
            if (count <= 0) return;
            if (__state.poisonPct < 0 || __state.poisonPct > 1) return;

            // allocate poison percents
            // The rationale for values' rounding is commented in SplitFoodPoisonablePostfix().
            int otherPoison = (int)Math.Round(otherTotal * otherPoisonPct);
            int targetPoison = (int)Math.Round(targetTotal * __state.poisonPct);

            #if DEBUG
                Log.Message(string.Format("Stack {0}/{1} at {2}% ({3}) is absorbing {4} pcs from stack {5}/{6} at {7}% ({8})",
                    targetPoison, targetTotal, __state.poisonPct * 100, __state.cause,
                    count, otherPoison, otherTotal, otherPoisonPct * 100, otherComp.cause));
            #endif

            // Number of poisoned items moved to the target stack.
            // Hypergeometric distribution; sample randomly from other stack without replacement.
            int splitPoison = Hypergeometric.Generate(N: otherTotal, K: otherPoison, n: count);
            // Number of poisoned items left in the other stack.
            otherPoison -= splitPoison;
            // Number of poisoned items to be in the target stack.
            int targetFinalPoison = targetPoison + splitPoison;

            // update target poison props
            ___poisonPct = (float)targetFinalPoison / (targetTotal + count);
            // Set an appropriate poisoning cause. In case of mixing equal quantities of poisoned items, inherit the cause from other stack, just like original game code does.
            __instance.cause = (targetFinalPoison == 0 ? FoodPoisonCause.Unknown : (targetPoison > splitPoison ? __state.cause : otherComp.cause));

            // update other poison props
            otherPoisonPct = (float)otherPoison / (otherTotal - count);
            typeof(CompFoodPoisonable).GetField("poisonPct", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(otherComp, otherPoisonPct);
            if (otherPoison == 0) otherComp.cause = FoodPoisonCause.Unknown;

            #if DEBUG
                Log.Message(string.Format("Absorbed: {0}/{1} at {2}% ({3}) after absorbing {4}/{5} and leaving {6}/{7} at {8}% ({9})",
                    targetFinalPoison, targetTotal + count, ___poisonPct * 100, __instance.cause,
                    splitPoison, count,
                    otherPoison, otherTotal - count, otherComp.PoisonPercent * 100, otherComp.cause));
            #endif
        }
    }
}
