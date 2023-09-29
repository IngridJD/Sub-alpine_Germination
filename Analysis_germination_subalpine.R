SeedDF$Strategy <- factor(SeedDF$Strategy, levels = c("Con", "Acq"))

# Model proportion emerged:
ModPropEmerged <- glmer(cbind(TotEmerged, TotNotEmerged) ~ OTC + Vegetation + Strategy + (Precip + I(Precip^2)) 
                        + OTC:Vegetation + OTC:Strategy + OTC:(Precip + I(Precip^2)) + Vegetation:Strategy + Vegetation:(Precip + I(Precip^2)) + Strategy:(Precip + I(Precip^2)) 
                        #                    + OTC:Vegetation:Strategy 
                        #                    + OTC:Vegetation:(Precip + I(Precip^2)) 
                        #                    + OTC:Strategy:(Precip + I(Precip^2)) 
                        + Vegetation:Strategy:(Precip + I(Precip^2))
                        + (1|SiteID) + (1|Species), family = binomial(link = "logit"), data = SeedDF)
while (length(ModPropEmerged@optinfo$conv$lme4) > 0) {
  pars = getME(ModPropEmerged,c("theta","fixef"))
  ModPropEmerged <-
    update(ModPropEmerged,
           start = pars,
           control = glmerControl(optCtrl = list(maxfun = 3e7)))
}

# Summary proportion emerged:
summary(ModPropEmerged)




# Model proportion survived:
ModPropSurvived <- glmer(cbind(SurvivedPlot, NotSurvivedPlot) ~ OTC + Vegetation + Strategy + (Precip + I(Precip^2)) 
                         + OTC:Vegetation + OTC:Strategy + OTC:(Precip + I(Precip^2)) + Vegetation:Strategy + Vegetation:(Precip + I(Precip^2)) + Strategy:(Precip + I(Precip^2)) 
                         + OTC:Vegetation:Strategy 
                         #                     + OTC:Vegetation:(Precip + I(Precip^2)) 
                         + OTC:Strategy:(Precip + I(Precip^2)) 
                         #                     + Vegetation:Strategy:(Precip + I(Precip^2))
                         + (1|SiteID) + (1|Species), family = binomial(link = "logit"), data = SeedDF)
while (length(ModPropSurvived@optinfo$conv$lme4) > 0) {
  pars = getME(ModPropSurvived,c("theta","fixef"))
  ModPropSurvived <-
    update(ModPropSurvived,
           start = pars,
           control = glmerControl(optCtrl = list(maxfun = 3e8)))
}

# Summary proportion survived:
summary(ModPropSurvived)



# Model T50% emergence:
ModT50 <- glmer(T50 ~ OTC + Vegetation + Strategy + (Precip + I(Precip^2)) 
                + OTC:Vegetation + OTC:Strategy + OTC:(Precip + I(Precip^2)) + Vegetation:Strategy + Vegetation:(Precip + I(Precip^2)) + Strategy:(Precip + I(Precip^2)) 
                #      + OTC:Vegetation:Strategy
                + OTC:Vegetation:(Precip + I(Precip^2)) 
                + OTC:Strategy:(Precip + I(Precip^2)) 
                + Vegetation:Strategy:(Precip + I(Precip^2)) 
                + OTC:Vegetation:Strategy:(Precip + I(Precip^2)) 
                + (1|SiteID) + (1|Species), family = poisson(link = "log"), data = SeedDF)

# Summary T50%:
summary(ModT50)

