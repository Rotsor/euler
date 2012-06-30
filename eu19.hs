import Data.Time
import Data.Time.Calendar.WeekDate

solution = filter (\(_,_,s) -> s==7) $ [toWeekDate $ fromGregorian y m 1 | y <- [1901..2000], m<-[1..12]]

main = print $ length solution