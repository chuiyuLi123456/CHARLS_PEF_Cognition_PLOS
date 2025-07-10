library(haven)
library(tidyverse)
library(charlsMAX)
library(knitr)
library(kableExtra)
library(dplyr)
colnames(df)


design <- c(
  # 一开始加入一个2015的原始数据确定人数
  "householdID_Demographic_Background_2015",
  "r3cesd10_H_CHARLS_D_Data",
  "r3puff_H_CHARLS_D_Data",#最大PEF
  "r3mbmi_H_CHARLS_D_Data",#bmi
  "r3agey_H_CHARLS_D_Data",#年龄
  "ragender_H_CHARLS_D_Data",#性别
  "raeduc_c_H_CHARLS_D_Data",#教育程度
  "r3mstath_H_CHARLS_D_Data",#婚姻状态
  "h3rural_H_CHARLS_D_Data",#居住地
  "r3smokev_H_CHARLS_D_Data",#是否吸烟
  "r3drinkev_H_CHARLS_D_Data",#是否喝酒
  #CESD-8
  'r3depresl_H_CHARLS_D_Data','r3effortl_H_CHARLS_D_Data',
  'r3sleeprl_H_CHARLS_D_Data','r3flonel_H_CHARLS_D_Data',
  'r3botherl_H_CHARLS_D_Data','r3goingl_H_CHARLS_D_Data',
  'r3mindtsl_H_CHARLS_D_Data','r3fearll_H_CHARLS_D_Data',
  # chronic  14种  
  "r3hibpe_H_CHARLS_D_Data", "r3diabe_H_CHARLS_D_Data", "r3cancre_H_CHARLS_D_Data", "r3lunge_H_CHARLS_D_Data", "r3hearte_H_CHARLS_D_Data", "r3stroke_H_CHARLS_D_Data", "r3psyche_H_CHARLS_D_Data", "r3arthre_H_CHARLS_D_Data", "r3dyslipe_H_CHARLS_D_Data", "r3livere_H_CHARLS_D_Data", "r3kidneye_H_CHARLS_D_Data", "r3digeste_H_CHARLS_D_Data", "r3asthmae_H_CHARLS_D_Data", "r3memrye_H_CHARLS_D_Data",
  # functional limit  ALD   IADL
  "r3adlab_c_H_CHARLS_D_Data", "r3iadlza_H_CHARLS_D_Data",
  # mental # 2015   年月日周几  减7  复制绘图  
  "r3orient_H_CHARLS_D_Data", "r3ser7_H_CHARLS_D_Data","r3draw_H_CHARLS_D_Data",
  # mental # 2018
  "r4orient_H_CHARLS_D_Data", "r4ser7_H_CHARLS_D_Data","r4draw_H_CHARLS_D_Data",
  # immediate+delayed 2015 
  "r3tr20_H_CHARLS_D_Data",
  # immediate+delayed 2018
  "r4tr20_H_CHARLS_D_Data",
  "da051_random_Health_Status_and_Functioning_2015",
  "hh1itot_H_CHARLS_D_Data",
  "dc017_Health_Status_and_Functioning_2015",
  'MMSE_cognitive_score_preset_w3',
  "MMSE_cognitive_score_preset_w4"
  
)

column_names <- get_descriptions(design)


# 获取原始数据
df <- fetch_CHARLS_data(design, merge_method ="left",column_names)
colnames(df_filtered )
df=df1
#筛选年龄大于60岁
df <- df %>% 
  filter(r3agey_w3_r_age_in_years >= 60)
df2
# 删除任一列为 "1.Yes" 的行
df_filtered <- df %>% 
  filter(
    !(
      `r3lunge_w3_r_ever_had_lung_disease` == "1.Yes" |
        `r3asthmae_w3_r_ever_had_asthma` == "1.Yes" |
        `r3memrye_w3_r_ever_had_memory_problem` == "1.Yes" |
        `r3psyche_w3_r_ever_had_psych_problem` == "1.Yes"
    )
  )

df2=df
df=df_filtered 
step1 <- df_filtered %>%transmute(ID=ID,
                        PEF=r3puff_w3_r_maximum_lung_function_peak_flow,
                         age= r3agey_w3_r_age_in_years,
                         CESD=r3cesd10_w3_r_cesd_score,
                         gender = case_when(
                           ragender_r_gender == "1.man" ~ "male",
                           ragender_r_gender == "2.woman" ~ "female"),#性别
                         marital = case_when(
                           r3mstath_w3_r_marital_status %in% c("1.married", "2.married, sp abs") ~ "married",
                           r3mstath_w3_r_marital_status %in% c("3.partnered", "4.separated","5.divorced", "7.widowed", "8.never married") ~ "single"),#已婚/单身
                         smoking = case_when(
                           r3smokev_w3_r_smoke_ever == "1.Yes" ~ "yes",
                           r3smokev_w3_r_smoke_ever == "0.No" ~ "no"),#是否吸烟
                         drinking = case_when(
                           r3drinkev_w3_r_ever_drinks_any_alcohol_before == "1.Yes" ~ "yes",
                           r3drinkev_w3_r_ever_drinks_any_alcohol_before == "0.None" ~ "no"),#是否喝酒
                         education_level = case_when(
                           raeduc_c_r_education_ %in% c("1.No formal education illiterate") ~ "illiterate",
                           raeduc_c_r_education_ %in% c("2.Did not finish primary school but capable of reading and/or writing", 
                                                        "3.Sishu",
                                                        "4.Elementary school") ~ "Primary school",
                           raeduc_c_r_education_ == "5.Middle school" ~ "Middle school",
                           raeduc_c_r_education_ %in% c("6.High school", 
                                                        "7.Vocational school", 
                                                        "8.Two/Three Year College/Associate degree",
                                                        "9.Four Year College/Bachelor's degree", 
                                                        "10.Post-graduated(Master/PhD)") ~ "High school and above"
                         ),#教育程度四分类
                         felt_depressed= case_when(
                           r3depresl_w3_r_cesd__felt_depressed == "1.Rarely or none of the time < 1 day" ~ 1,
                           r3depresl_w3_r_cesd__felt_depressed == "2.Some or a little of the time 1-2 days" ~ 2,
                           r3depresl_w3_r_cesd__felt_depressed == "3.Occasionally or a moderate amount of 3" ~ 3,
                           r3depresl_w3_r_cesd__felt_depressed == "4.Most or all of the time 5-7 days" ~ 4),
                         everything_an_effort= case_when(
                           `r3effortl_w3_r_cesd__everything_an_effort` == "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3effortl_w3_r_cesd__everything_an_effort` == "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3effortl_w3_r_cesd__everything_an_effort` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3effortl_w3_r_cesd__everything_an_effort` == "4.Most or all of the time 5-7 days" ~ 4),
                         sleep_was_restless= case_when(
                           `r3sleeprl_w3_r_cesd__sleep_was_restless` == "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3sleeprl_w3_r_cesd__sleep_was_restless` == "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3sleeprl_w3_r_cesd__sleep_was_restless` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3sleeprl_w3_r_cesd__sleep_was_restless` == "4.Most or all of the time 5-7 days" ~ 4),
                         felt_lonely= case_when(
                           `r3flonel_w3_r_cesd__felt_lonely` == "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3flonel_w3_r_cesd__felt_lonely` == "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3flonel_w3_r_cesd__felt_lonely` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3flonel_w3_r_cesd__felt_lonely` == "4.Most or all of the time 5-7 days" ~ 4),
                         bothered_by_little_things= case_when(
                           `r3botherl_w3_r_cesd__bothered_by_little_things` == "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3botherl_w3_r_cesd__bothered_by_little_things` == "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3botherl_w3_r_cesd__bothered_by_little_things` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3botherl_w3_r_cesd__bothered_by_little_things` == "4.Most or all of the time 5-7 days" ~ 4),
                         could_not_get_going= case_when(
                           `r3goingl_w3_r_cesd__could_not_get_going` == "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3goingl_w3_r_cesd__could_not_get_going` == "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3goingl_w3_r_cesd__could_not_get_going` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3goingl_w3_r_cesd__could_not_get_going` == "4.Most or all of the time 5-7 days" ~ 4),
                         had_trouble_keeping_mind_on_what_is_doing= case_when(
                           `r3mindtsl_w3_r_cesd__had_trouble_keeping_mind_on_what_is_doing` == "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3mindtsl_w3_r_cesd__had_trouble_keeping_mind_on_what_is_doing`== "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3mindtsl_w3_r_cesd__had_trouble_keeping_mind_on_what_is_doing` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3mindtsl_w3_r_cesd__had_trouble_keeping_mind_on_what_is_doing` == "4.Most or all of the time 5-7 days" ~ 4),
                         feel_fearful= case_when(
                           `r3fearll_w3_r_cesd__feel_fearful`== "1.Rarely or none of the time < 1 day" ~ 1,
                           `r3fearll_w3_r_cesd__feel_fearful`== "2.Some or a little of the time 1-2 days" ~ 2,
                           `r3fearll_w3_r_cesd__feel_fearful` == "3.Occasionally or a moderate amount of 3" ~ 3,
                           `r3fearll_w3_r_cesd__feel_fearful`== "4.Most or all of the time 5-7 days" ~ 4),#抑郁症
                         fen= `felt_depressed` + `everything_an_effort` +
                           `sleep_was_restless` + `felt_lonely` + 
                           `bothered_by_little_things`+ `could_not_get_going` + 
                           `had_trouble_keeping_mind_on_what_is_doing` + `feel_fearful` ,#抑郁症总分
                        
                        
                         sleep= case_when(
                           i_felt_lonely. == "1 Rarely or None of The Time <1 Day" ~ 1,
                           i_felt_lonely. == "2 Some or A Little of The Time 1-2 Days" ~ 2,
                           i_felt_lonely. == "3 Occasionally or A Moderate Amount of The Time 3-4 Days" ~ 3,
                           i_felt_lonely. == "4 Most or All of The Time 5-7 Days" ~ 4),
                         
                         Family_residence = case_when(
                           h3rural_w3_lives_in_rural_or_urban == "1.Rural Village" ~ "Rural",
                           h3rural_w3_lives_in_rural_or_urban == "0.Urban Community" ~ "Urban"),#城市  乡村
                         bmi = case_when(
                           r3mbmi_w3_r_measured_body_mass_index_ < 18.5 ~ "Underweight",      # 低于18.5为体重过轻
                           r3mbmi_w3_r_measured_body_mass_index_ >= 18.5 & r3mbmi_w3_r_measured_body_mass_index_ < 24.9 ~ "Normal",  # 18.5到24.9为正常
                           r3mbmi_w3_r_measured_body_mass_index_ >= 25 & r3mbmi_w3_r_measured_body_mass_index_ < 29.9 ~ "Overweight", # 25到29.9为超重
                           r3mbmi_w3_r_measured_body_mass_index_ >= 30 ~ "Obesity",           # 30及以上为肥胖
                           TRUE ~ NA_character_  # 如果没有匹配的情况，设为NA
                         ),#BMI四分类
                         #计算个体患有的慢性疾病的总数
                        chronic = rowSums(
                          across(
                            all_of(c("r3hibpe_w3_r_ever_had_high_blood_pressure",
                                     "r3diabe_w3_r_ever_had_diabetes",
                                     "r3cancre_w3_r_ever_had_cancer",
                              
                                     "r3hearte_w3_r_ever_had_heart_problem",
                                     "r3stroke_w3_r_ever_had_stroke",
                                    
                                     "r3arthre_w3_r_ever_had_arthritis",
                                     "r3dyslipe_w3_r_ever_had_dyslipidemia",
                                     "r3livere_w3_r_ever_had_liver_disease",
                                     "r3kidneye_w3_r_ever_had_kidney_disease",
                                     "r3digeste_w3_r_ever_had_stomach_digestive_disease"
                                   )),
                            ~ as.character(.)
                          ) == "1.Yes", 
                          na.rm = TRUE
                        ),
                        
                                                  #select(.) 表示选择当前数据框中的多个指定列
                                                  #rowSums() 函数用于计算每一行中 TRUE 的数量。TRUE 会被视为 1，FALSE 会被视为 0
                         Number_of_chronic_conditions = case_when(
                           chronic == 0 ~ "0",
                           chronic == 1 ~ "1",
                           chronic >= 2 ~ "≥2"),#慢病数量3分类
                        
                        
                        
                        Func_limt = rowSums(
                          across(
                            c(`r3adlab_c_w3_r_some_diff-6_item_adl_scale`,
                              `r3iadlza_w3_r_some_diff-iadls_summary__0-5`),
                            ~ as.numeric(.)
                          ),
                          na.rm = TRUE
                        ),
                  
                         cognition_2015 = r3orient_w3_r_cognition_orient_ +r3ser7_w3_r_serial_7s+r3draw_w3_r_cognition_able_to_draw_assign_picture+r3tr20_w3_r_recall_summary_score/2,
                         cognition_2018 = r4orient_w4_r_cognition_orient_ +r4ser7_w4_r_serial_7s+r4draw_w4_r_cognition_able_to_draw_assign_picture+r4tr20_w4_r_recall_summary_score/2,
                     MMSE3=mmse_cognitive_score_1,
                     MMSE4=mmse_cognitive_score_2
                     
)
#%>%filter(!is.na(CESD),!is.na(cognition_2018),!is.na(PEF))
colnames(step1)
step1_filtered <- step1 %>% 
  filter(!is.na(CESD), !is.na(MMSE4), !is.na(PEF))
step3_filtered <- step1 %>% 
  filter(!is.na(cognition_2018))

step4_filtered <- step1 %>% 
  filter(!is.na(PEF))

step_zong<- merge(step1, step4, by='ID', all.x = T)
step1=step_zong

step1=step1_filtered


step1_filtered1 <- step2 %>% 
  filter(!is.na(CESD), !is.na(MMSE4), !is.na(PEF))


step5_filtered <- step_zong %>% 
  filter(!is.na(mmse_cognitive_score))


colnames(step1)
#删去无用列
step1 <- step1 %>% 
  select(-any_of(c("chronic", "Func_limt", "felt_depressed", "everything_an_effort",
                   "sleep_was_restless", "felt_lonely", "bothered_by_little_things",
                   "could_not_get_going", "had_trouble_keeping_mind_on_what_is_doing",
                   "feel_fearful", "r3draw_w3_r_cognition_able_to_draw_assign_picture",
                   "r4draw_w4_r_cognition_able_to_draw_assign_picture")))

step2 <- step1_filtered %>% 
  dplyr::select(-all_of(names(step1_filtered)[c(10:19, 22,24,25,26,27,28)]))


write.csv(step2, "step2.csv", row.names = FALSE)

write.csv(step1, "2320删除Na.csv", row.names = FALSE)

step1=step2

step1 <- na.omit(step1)
#step2是要纳入分析的数据

step21 <- subset(step2, select = -MMSE3)
step211 <- na.omit(step21)

step1=step211




colnames(step21)
which(is.na(step2),arr.ind =TRUE)


# 检查原始数据的缺失值情况
summary(step21)


colnames(step1)
####1.基线表####
library(tableone)
table1 <- CreateTableOne(vars = colnames(step1)[c(2:4,6:14)],
                         strata = "gender", # 指定分组变量
                         data = step1,
                         factorVars = c('education_level','Family_residence','marital','Number_of_chronic_conditions','bmi','smoking','drinking'), # 指定分类变量
                         addOverall = T, #是否计算汇总人群
                         smd = F)# 不应计算表格的标准化均值差异
table_final <- print(table1, showAllLevels = TRUE, # 显示分类变量的所有水平
                     exact = "hispan" , # 指定需要使用fisher精确检验的变量
                     pDigits = 3, # P值小数位数为4位
                     catDigits = 1,  # 分类变量百分比小数位数为1位
                     contDigits = 1,
                     smd = F) # 连续变量小数位数为1位

# 生成表格
kable(table_final, 
      format = "html",  
      col.names = c("Variable", "Overall", "Female", "Male", "P-value", "Test"),  
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F, 
                position = "center") %>%
  add_header_above(c(" " = 1, "Stratified by gender" = 6)) %>%  
  row_spec(0, bold = TRUE)  


####2.spearman correlation#只挑出来没有na的做####
  GGally::ggpairs(iris)#对角线上是1
corr_data <- step1 %>% select(PEF,CESD,MMSE4)%>%na.omit()##选出这三个数据
  #三者均不缺失的只有corr_data=2711人
corr_data <- step1 %>% 
  dplyr::select(PEF, CESD, MMSE4) %>% 
  na.omit()
#逐个分析，比较麻烦
#cor(corr_data$grip,corr_data$Func_limt)#grip 列和 Func_limt 列之间的相关性
#cor(corr_data$Func_limt,corr_data$MMSE4#功能限制与随访认知之间相关性（线性）
corr_data <- step1 %>% 
  dplyr::select(PEF, CESD, MMSE4) %>% 
  na.omit()
library(plyr)


#合起来一块算
library(psych)
corr.test(corr_data)###计算相关性；多个两两间比较，自动进行了多重校正;显示为0表示很小，即相关性的pval显著

# 计算阈值（均值减去标准差）
threshold <- mean(step1$MMSE3) - sd(step1$MMSE3)

# 过滤掉 MMSE3 列中低于或等于该阈值的行
filtered_df <- subset(step1, MMSE3 > threshold)







####3.线性回归，认知-握力+/-功能限制+协变量####
library(lm.beta)
fit<-lm(MMSE4~+CESD+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,data=step1)
fit_beta<-lm.beta(fit)#标准化beta
summary(fit)

###4.中介分析--总效应、
#第一步：构建原始的模型（结局~暴露+协变量）认知-握力++协变量【自变量-因变量，总效应】
fit.totaleffect<-lm(MMSE4~PEF+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,data=step1)
summary(fit.totaleffect)
library(lm.beta)
lm.fit.totaleffect.std<-lm.beta(fit.totaleffect)
summary(lm.fit.totaleffect.std)



#第二步：构建中介变量模型（中介~暴露+协变量）功能限制-握力++协变量【自变量-中介，此步的效应值必须显著】
fit.mediator=lm(CESD~PEF+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,step1)
summary(fit.mediator)
lm.fit.mediator.std<-lm.beta(fit.mediator)
summary(lm.fit.mediator.std)


#第三步：构建完整的模型（结局~暴露+中介变量+协变量）认知-握力+功能限制++协变量【自变量/中介变量-因变量】
fit.dv=lm(MMSE4~PEF+CESD+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,step1)
summary(fit.dv)
lm.fit.dv.std<-lm.beta(fit.dv)
summary(lm.fit.dv.std)




library(mediation)###中介的包
res = mediate(fit.mediator,fit.dv, treat='PEF',mediator = 'CESD',boot=T)
summary(res)
#fit.mediator: 指的是包含中介变量 Func_limt 作为因变量的线性模型。该模型应该解释 grip 如何影响 Func_limt。
#fit.dv: 指的是将认知功能作为因变量的线性模型，它应该包括 grip 和 Func_limt。
#treat='grip': 这里 grip 被视为治疗或干预变量。
#mediator='Func_limt': 这里 Func_limt 被视为中介变量。
#boot=T: 启用自举（bootstrap）方法来估计效应的置信区间，这是一种通过重复抽样计算统计量的方法，有助于评估估计的可靠性。
#ACME：间接效应  
#ADE：直接效应  
#Total Effect：总效应=直接+间接  
#Prop. Mediated中介效应比例

#filtered_df
write.csv(filtered_df, "去掉一个标准差底下的量.csv", row.names = FALSE)
#敏感性分析
###4.中介分析--总效应、
#第一步：构建原始的模型（结局~暴露+协变量）认知-握力++协变量【自变量-因变量，总效应】
fit.totaleffect<-lm(MMSE4~PEF+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,data=filtered_df)
summary(fit.totaleffect)
library(lm.beta)
lm.fit.totaleffect.std<-lm.beta(fit.totaleffect)
summary(lm.fit.totaleffect.std)



#第二步：构建中介变量模型（中介~暴露+协变量）功能限制-握力++协变量【自变量-中介，此步的效应值必须显著】
fit.mediator=lm(CESD~PEF+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,filtered_df)
summary(fit.mediator)
lm.fit.mediator.std<-lm.beta(fit.mediator)
summary(lm.fit.mediator.std)


#第三步：构建完整的模型（结局~暴露+中介变量+协变量）认知-握力+功能限制++协变量【自变量/中介变量-因变量】
fit.dv=lm(MMSE4~PEF+CESD+age+gender+education_level+Family_residence+marital+Number_of_chronic_conditions+smoking+drinking+bmi,filtered_df)
summary(fit.dv)
lm.fit.dv.std<-lm.beta(fit.dv)
summary(lm.fit.dv.std)




library(mediation)###中介的包
res = mediate(fit.mediator,fit.dv, treat='PEF',mediator = 'CESD',boot=T)
summary(res)
#fit.mediator: 指的是包含中介变量 Func_limt 作为因变量的线性模型。该模型应该解释 grip 如何影响 Func_limt。
#fit.dv: 指的是将认知功能作为因变量的线性模型，它应该包括 grip 和 Func_limt。
#treat='grip': 这里 grip 被视为治疗或干预变量。
#mediator='Func_limt': 这里 Func_limt 被视为中介变量。
#boot=T: 启用自举（bootstrap）方法来估计效应的置信区间，这是一种通过重复抽样计算统计量的方法，有助于评估估计的可靠性。
#ACME：间接效应  
#ADE：直接效应  
#Total Effect：总效应=直接+间接  
#Prop. Mediated中介效应比例


