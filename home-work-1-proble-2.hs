-- students :: [Student]
-- students = [
--   ("Ioan", "Stoianov", "81596", 1),
--   ("Nikolay", "Petrov", "12345", 2),
--   ("Viktor", "Ivanov", "23456", 2),
--   ("Monika", "Beluci", "34567", 2),
--   ("Maria", "Antoaneta", "45678", 2),
--   ("Dayana", "Ivanova", "56789", 2)]

-- studentScores :: [StudentScore]
-- studentScores = [("81596", 3.0, "1"),("12345", 4.0, "1"),("23456", 4.0, "1"),
--                 ("34567", 6.0, "2"),("45678", 6.0, "2"),("56789", 5.0, "2")]

-- disciplines :: [Discipline]
-- disciplines = [("FP", "1", 2000),("SDP", "2", 2000)]
type Fn = String  -- факултетен номер на студент
type FirstName = String -- първо име на студент
type LastName = String -- последно име на студент
type Score = Float -- оценка
type DisciplineID = String -- идентификационен номер на дисциплина
type Course = Integer -- курс на студент
type DisciplineName = String -- име на съотдетна дисциплина
type CourseYear = Integer -- година от обучението за която е подходяща съответна дисциплина 

type StudentScore = (Fn, Score, DisciplineID)
type Student = (FirstName, LastName, Fn, Course)
type Discipline = (DisciplineName, DisciplineID, CourseYear)

getStudentScoreFn:: StudentScore -> Fn
getStudentScoreFn (fn, _, _) = fn

isPresentDisciplineId :: DisciplineID -> StudentScore -> Bool
isPresentDisciplineId dID (_, _, disciplineId) = disciplineId == dID

filterStutentScoresByDisciplineID :: [StudentScore] -> DisciplineID -> [StudentScore]
filterStutentScoresByDisciplineID studentScores disciplineID =
    filter (isPresentDisciplineId disciplineID) studentScores

isPresentFn :: [Fn] -> Student -> Bool
isPresentFn fns (_, _,fn, _) = fn `elem` fns

getStudentNames :: Student -> (FirstName, LastName)
getStudentNames (fName, lName, _, _) = (fName, lName)
    
filterStutentsByScoreFn :: [StudentScore] -> [Student] -> [(FirstName, LastName)]
filterStutentsByScoreFn studentScores students = do
  let filteredScores = filter (isPresentFn fns) students
  map getStudentNames filteredScores
  where fns = map getStudentScoreFn studentScores

getNamesByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> [(FirstName, LastName)]
getNamesByDisciplineID studentScores students disciplineID = do
    let filteredScoresByDisciplineID = filterStutentScoresByDisciplineID studentScores disciplineID
    filterStutentsByScoreFn filteredScoresByDisciplineID students

getScore :: StudentScore -> Score
getScore (_, score, _) = score

getAvg:: DisciplineID -> [StudentScore] -> Float
getAvg disciplineId studentScores = do
  let filteredScores = filterStutentScoresByDisciplineID studentScores disciplineId
  let scoreList = map getScore filteredScores
  sum scoreList / fromIntegral (length scoreList)

getDisciplineNamesAndAvgScore :: [StudentScore]-> Discipline ->  (DisciplineName, DisciplineID, Float)
getDisciplineNamesAndAvgScore studentScores (disciplineName, disciplineId, _)  =
  (disciplineName, disciplineId, getAvg disciplineId studentScores)

getResults :: [StudentScore] -> [Discipline] -> [(DisciplineName, DisciplineID, Float)]
getResults studentScores = map (getDisciplineNamesAndAvgScore studentScores)

isPresentFnInStudentScore:: Fn -> StudentScore -> Bool
isPresentFnInStudentScore fn (scoreFn, _, _) = fn == scoreFn

findAvgForDisciplineId:: DisciplineID -> (DisciplineName, DisciplineID, Float) -> Bool
findAvgForDisciplineId dID (_, avgId, _) = dID == avgId 


isScoreBeneathAverage :: [(DisciplineName, DisciplineID, Float)] -> StudentScore -> Bool
isScoreBeneathAverage avgScores (_, score, disciplineID) = do
  let (_, _, avgScore) = head $ filter (findAvgForDisciplineId disciplineID) avgScores
  avgScore > score

isStudentAboveAverage :: [StudentScore] -> [(DisciplineName, DisciplineID, Float)] -> Student -> Bool
isStudentAboveAverage studentScores avgScores (_,_, fn, _) = do
  let filteredStudentScores = filter (isPresentFnInStudentScore fn) studentScores
  let scoresBeneathAverage = filter (isScoreBeneathAverage avgScores) filteredStudentScores
  null scoresBeneathAverage

getAboveAvarageStudents :: [StudentScore] -> [Student] -> [Discipline] -> [Student]
getAboveAvarageStudents studentScores students disciplines = do
  let avgScores = getResults studentScores disciplines
  filter (isStudentAboveAverage studentScores avgScores)students