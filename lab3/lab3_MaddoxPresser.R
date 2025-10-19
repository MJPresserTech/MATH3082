# Lab 3 - Maddox Presser

# 1 a) bernoulli vectors
set.seed(69421)
x = sample(0:1, 500, replace = TRUE)
y = sample(0:1, 500, replace = TRUE)
table = table(x, y)
table_prob = table / 500
table_prob
prob_xeq0 = sum(table_prob["0", ]) # probability x=0
prob_xeq1 = sum(table_prob["1", ]) # probability x=1
prob_yeq0 = sum(table_prob[, "0"]) # probability y=0
prob_yeq1 = sum(table_prob[, "1"]) # probability y=1
multiplicative_prob_xeq0_yeq0 = prob_xeq0 * prob_yeq0 # P(X=0 and Y=0) = P(X=0)*P(Y=0) if independent
multiplicative_prob_xeq0_yeq1 = prob_xeq0 * prob_yeq1 # P(X=0 and Y=1) = P(X=0)*P(Y=1) if independent
multiplicative_prob_xeq1_yeq0 = prob_xeq1 * prob_yeq0 # P(X=1 and Y=0) = P(X=1)*P(Y=0) if independent
multiplicative_prob_xeq1_yeq1 = prob_xeq1 * prob_yeq1 # P(X=1 and Y=1) = P(X=1)*P(Y=1) if independent
prob_xeq0_yeq0 = table_prob["0", "0"] # P(X=0 and Y=0)
prob_xeq0_yeq1 = table_prob["0", "1"] # P(X=0 and Y=1)
prob_xeq1_yeq0 = table_prob["1", "0"] # P(X=1 and Y=0)
prob_xeq1_yeq1 = table_prob["1", "1"] # P(X=1 and Y=1)

joint_prob_table = data.frame(
  x = c(0, 0, 1, 1),
  y = c(0, 1, 0, 1),
  Multiplicative = c(multiplicative_prob_xeq0_yeq0, multiplicative_prob_xeq0_yeq1, multiplicative_prob_xeq1_yeq0, multiplicative_prob_xeq1_yeq1),
  Observed = c(prob_xeq0_yeq0, prob_xeq0_yeq1, prob_xeq1_yeq0, prob_xeq1_yeq1)
)
joint_prob_table

# 2 a) hospital data
hospital = read.csv("lab3/HospitalData.csv")

H = hospital$Hospital
O = hospital$Outcome

table(H, O)
table_prob = table(H, O) / length(H)
table_prob
p_S_given_A = table_prob["A", "S"] / sum(table_prob["A", ]) # P(B=1 | A=1)
p_D_given_A = table_prob["A", "D"] / sum(table_prob["A", ]) # P(B=0 | A=1)
p_S_given_B = table_prob["B", "S"] / sum(table_prob["B", ]) # P(B=1 | A=0)
p_D_given_B = table_prob["B", "D"] / sum(table_prob["B", ]) # P(B=0 | A=0)

cond_prob_table = data.frame(
  Hospital = c("A", "A", "B", "B"),
  Outcome = c("S", "D", "S", "D"),
  Conditional_Probability = c(p_S_given_A, p_D_given_A, p_S_given_B, p_D_given_B)
)
cond_prob_table

# 4 a) Precipitation data

data = read.csv("lab3/precip2.csv")
occur = (data > 0)
head(occur)
occ = occur + 0 # this trick will turn the matrix into a numeric one
head(occ)

precip_table = table(occ[,1], occ[,2])
precip_table

precip_prob_table = precip_table / sum(precip_table)
precip_prob_table
p_B1_given_A1 = precip_prob_table["1", "1"] / sum(precip_prob_table["1", ]) # P(B=1 | A=1)
p_B0_given_A1 = precip_prob_table["1", "0"] / sum(precip_prob_table["1", ]) # P(B=0 | A=1)
p_B1_given_A0 = precip_prob_table["0", "1"] / sum(precip_prob_table["0", ]) # P(B=1 | A=0)
p_B0_given_A0 = precip_prob_table["0", "0"] / sum(precip_prob_table["0", ]) # P(B=0 | A=0)

cond_prob_table = data.frame(
  Station_A = c(1, 1, 0, 0),
  Station_B = c(1, 0, 1, 0),
  Conditional_Probability = c(p_B1_given_A1, p_B0_given_A1, p_B1_given_A0, p_B0_given_A0)
)
cond_prob_table


prob_Aeq0 = sum(precip_prob_table["0", ]) # probability x=0
prob_Aeq1 = sum(precip_prob_table["1", ]) # probability x=1
prob_Beq0 = sum(precip_prob_table[, "0"])
prob_Beq1 = sum(precip_prob_table[, "1"])
multiplicative_prob_Aeq0_Beq0 = prob_Aeq0 * prob_Beq0
multiplicative_prob_Aeq0_Beq1 = prob_Aeq0 * prob_Beq1
multiplicative_prob_Aeq1_Beq0 = prob_Aeq1 * prob_Beq0
multiplicative_prob_Aeq1_Beq1 = prob_Aeq1 * prob_Beq1
prob_Aeq0_Beq0 = precip_prob_table["0", "0"]
prob_Aeq0_Beq1 = precip_prob_table["0", "1"]
prob_Aeq1_Beq0 = precip_prob_table["1", "0"]
prob_Aeq1_Beq1 = precip_prob_table["1", "1"]

joint_precip_prob_table = data.frame(
  A = c(0, 0, 1, 1),
  B = c(0, 1, 0, 1),
  Multiplicative = c(multiplicative_prob_Aeq0_Beq0, multiplicative_prob_Aeq0_Beq1, multiplicative_prob_Aeq1_Beq0, multiplicative_prob_Aeq1_Beq1),
  Conditional_Probability = c(p_B1_given_A1, p_B0_given_A1, p_B1_given_A0, p_B0_given_A0),
  Observed = c(prob_Aeq0_Beq0, prob_Aeq0_Beq1, prob_Aeq1_Beq0, prob_Aeq1_Beq1)
)
joint_precip_prob_table