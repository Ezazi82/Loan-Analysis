Select *
From loans


--Looking at the number of loan applicants by Gender and Marriage Status
Select Gender, Married, Count(Married) as NumberMarried
From loans
Where Gender is not null and Married is not null
Group By Gender, Married
Order By Married Desc

--Average Loan Amount Given Based on Gender (not including 0's)
Select Gender, Avg(Loan_Amount) as AvgLoan
From loans
Where Gender is not null and Loan_Amount <> 0
Group By Gender
Order By Avg(Loan_Amount) Desc

--Determining Education and Employment of Applicants
Select Education, Self_Employed, Count(Self_Employed) as NumSelfEmployed
From loans
Where Education is not null and Self_Employed is not null
Group By Education, Self_Employed

--Determining if Education impacts Application Income (which might affect chances of loan approval)
Select Education, Avg(Applicant_Income) as AvgApplicantIncome
From loans
Group By Education


--Now lets look at who does and doesn't get loans

--By Area
Select Area, Status, Count(Status) as NumberLoansApproved
From loans
Group By Status, Area
Order By Area Desc

--By Term
Select Term, Status, Count(Status) as NumberLoansApproved
From loans
Where Term is not null
Group By Status, Term
Order by Status Desc

--By Gender (disproportionate amount of males apply for loans already, will run Hypothesis test in R)
Select Gender, Status, Count(Status)
From loans
Where Gender is not null
Group By Gender, Status
Order By Gender

--Creating a Temp Table to play a little more with the information
Drop Table if exists #temp_Loans
Create Table #temp_Loans (
Gender varchar(50),
Married varchar(50),
Dependents int,
Education varchar(50),
TotalIncome int,
Loan_Amount int,
Area varchar(50),
Status varchar(50)
)

Insert into #temp_Loans
Select Gender, Married, Dependents, Education, Sum(Applicant_Income + Coapplicant_Income), Loan_Amount, Area, Status
From loans
Group By Gender, Married, Dependents, Education, Loan_Amount, Area, Status



Select Married, Education, Avg(TotalIncome) as AvgTotalIncome, Count(Status) as NumMarriedGrad
From #temp_Loans
Where Gender is not null and Married is not null and Dependents is not null and Married = 'Yes'
Group By Married, Education
