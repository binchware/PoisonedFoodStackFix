using HarmonyLib;
using RimWorld;
using System;
using System.Reflection;
using Verse;

namespace PoisonedFoodStackFix
{
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

            // patch to record poison percent before merging stacks
            harmony.Patch(AccessTools.Method(
                    typeof(CompFoodPoisonable),
                    nameof(CompFoodPoisonable.PreAbsorbStack)),
                prefix: new HarmonyMethod(patchType, nameof(AbsorbStackPoisonablePrefix)));

            // patch to better distribute poison percent when merging stacks
            harmony.Patch(AccessTools.Method(
                    typeof(CompFoodPoisonable),
                    nameof(CompFoodPoisonable.PreAbsorbStack)),
                postfix: new HarmonyMethod(patchType, nameof(AbsorbStackPoisonablePostfix)));
        }

        public static void SplitFoodPoisonablePostfix(CompFoodPoisonable __instance, ref float ___poisonPct, Thing piece)
        {
            // verify expectations
            // - base game behavior is to give split stack same poisonPct as base stack
            // - if this isn't the case, then base game behavior has changed or another mod is handling this
            CompFoodPoisonable compFoodPoisonable = piece.TryGetComp<CompFoodPoisonable>();
            if (___poisonPct != compFoodPoisonable.PoisonPercent) return;

            // skip if entire base stack is poisoned
            // - base game behavior should be adequate
            if (__instance.PoisonPercent == 1f) return;

            // skip if either stack is empty
            // - base game behavior should be adequate
            if (piece.stackCount == 0 || __instance.parent.stackCount == 0) return;

            // allocate poison percents
            // - hypergeometric distribution; sample randomly from base stack without replacement
            int baseTotal = __instance.parent.stackCount + piece.stackCount;
            int basePoison = (int)Math.Round(baseTotal * __instance.PoisonPercent);
            int splitPoison = 0;
            for (int i = 0; i < piece.stackCount; ++i)
            {
                if (basePoison == 0)
                    break;

                if (Rand.Chance((float)basePoison / baseTotal))
                {
                    --basePoison;
                    ++splitPoison;
                }

                --baseTotal;
            }

            // update split poison props
            float splitPoisonPct = (float)splitPoison / piece.stackCount;
            typeof(CompFoodPoisonable).GetField("poisonPct", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(compFoodPoisonable, splitPoisonPct);
            if (splitPoison == 0)
            {
                typeof(CompFoodPoisonable).GetField("cause", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(compFoodPoisonable, FoodPoisonCause.Unknown);
            }

            // update base poison props
            ___poisonPct = (float)basePoison / __instance.parent.stackCount;
            if (basePoison == 0)
            {
                typeof(CompFoodPoisonable).GetField("cause", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(__instance, FoodPoisonCause.Unknown);
            }
        }

        public static void AbsorbStackPoisonablePrefix(CompFoodPoisonable __instance, float ___poisonPct, out float __state)
        {
            // record original poisonPct
            __state = ___poisonPct;
        }

        public static void AbsorbStackPoisonablePostfix(CompFoodPoisonable __instance, ref float ___poisonPct, Thing otherStack, int count, float __state)
        {
            // skip if entire other stack is poisoned
            // - base game behavior should be adequate
            CompFoodPoisonable compFoodPoisonable = otherStack.TryGetComp<CompFoodPoisonable>();
            if (compFoodPoisonable.PoisonPercent == 1f) return;

            // skip if we're absorbing entire other stack
            // - base game behavior should be adequate
            if (otherStack.stackCount == count) return;

            // verify expectations
            // - base game behavior is to set poisonPct based on weighted average
            // - if this isn't the case, then base game behavior has changed or another mod is handling this
            float weightedAvg = GenMath.WeightedAverage(__state, __instance.parent.stackCount, compFoodPoisonable.PoisonPercent, count);
            if (___poisonPct != weightedAvg) return;

            // allocate poison percents
            // - hypergeometric distribution; sample randomly from other stack without replacement
            int otherTotal = otherStack.stackCount;
            int otherPoison = (int)Math.Round(otherTotal * compFoodPoisonable.PoisonPercent);
            int targetPoison = (int)Math.Round(__instance.parent.stackCount * __state);
            for (int i = 0; i < count; ++i)
            {
                if (otherPoison == 0 || otherTotal == 0)
                    break;

                if (Rand.Chance((float)otherPoison / otherTotal))
                {
                    --otherPoison;
                    ++targetPoison;
                }

                --otherTotal;
            }

            // update other poison props
            float otherPoisonPct = (float)otherPoison / (otherStack.stackCount - count);
            typeof(CompFoodPoisonable).GetField("poisonPct", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(compFoodPoisonable, otherPoisonPct);
            if (otherPoison == 0)
            {
                typeof(CompFoodPoisonable).GetField("cause", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(compFoodPoisonable, FoodPoisonCause.Unknown);
            }

            // update target poison props
            ___poisonPct = (float)targetPoison / (__instance.parent.stackCount + count);
            if (targetPoison == 0)
            {
                typeof(CompFoodPoisonable).GetField("cause", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(__instance, FoodPoisonCause.Unknown);
            }
        }
    }
}
