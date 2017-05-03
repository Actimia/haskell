import Data.List

data Month = Jan | Feb | Mar
    | Apr | May | Jun | Jul | Aug
    | Sep | Oct | Nov | Dec
    deriving (Show, Enum, Eq, Ord)

type Day = Int
type Year = Int
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Eq, Ord)
data Date = Date {year::Year, month::Month, day::Day, weekday::Weekday} deriving (Show, Eq, Ord)

start = Date 1901 Jan 01 Sun
days = takeWhile pred $ iterate next start
    where
        pred (Date y _ _ _) = y < 2001

p19 = length $ filter pred days
    where
        pred (Date _ _ 1 Sun) = True
        pred _ = False

next date@(Date y m d wd) = Date (nextYear date) (nextMonth date) (nextDay date) (nextWeekday date)

nextYear (Date y Dec 31 _) = y + 1
nextYear (Date y _ _ _) = y

isleapyear y = (mod y 4 == 0) && (mod y 100 /= 0 || mod y 400 == 0)

nextMonth (Date _ Jan 31 _) = Feb
nextMonth (Date y Feb 28 _) = if (isleapyear y) then Feb else Mar
nextMonth (Date _ Feb 29 _) = Mar
nextMonth (Date _ Mar 31 _) = Apr
nextMonth (Date _ Apr 30 _) = May
nextMonth (Date _ May 31 _) = Jun
nextMonth (Date _ Jun 30 _) = Jul
nextMonth (Date _ Jul 31 _) = Aug
nextMonth (Date _ Aug 31 _) = Sep
nextMonth (Date _ Sep 30 _) = Oct
nextMonth (Date _ Oct 31 _) = Nov
nextMonth (Date _ Nov 30 _) = Dec
nextMonth (Date _ Dec 31 _) = Jan
nextMonth (Date _ m _  _) = m

nextDay date@(Date _ m d _) = if (nextMonth date == m) then d + 1 else 1

nextWeekday (Date _ _ _ Sun) = Mon
nextWeekday (Date _ _ _ x) = succ x
