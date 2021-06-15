module Schedule exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)


type S
    = -- Seconds
      Seconds
    | SecondsFrom Int Int
    | Second Int
      --  Minutes
    | Minutes
    | MinutesFrom Int Int
    | Minute Int
      -- Hours
    | Midday
    | Hour Int
    | HoursFrom Int Int
      -- Days
    | Day Day
    | DaysFrom Day Day
    | DaysOfYear
      -- Months
    | MonthsFrom S S
    | Mar
    | Apr
    | Sep
    | Oct
      -- Operators
    | Every Int S
    | Group (List S)
    | Excluding S


type Day
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


dayToString d =
    case d of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


dayToInt d =
    case d of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6


intToDay i =
    case i of
        0 ->
            Mon

        1 ->
            Tue

        2 ->
            Wed

        3 ->
            Thu

        4 ->
            Fri

        5 ->
            Sat

        6 ->
            Sun

        _ ->
            Sun


main =
    output


output =
    div [ style "font-family" "SF Pro Display" ]
        [ examples
            |> List.map
                (\s ->
                    let
                        grid =
                            toGrid emptyGrid s
                    in
                    div [ style "padding" "5px" ]
                        [ div [ style "font-family" "monospace", style "background-color" "#ccc", style "display" "inline-block", style "padding" "5px" ] [ text <| Debug.toString s ]
                        , div [ style "font-weight" "bold" ] [ text <| "⏩ " ++ gridWords grid ]
                        , div [ style "color" "#eee" ] [ text <| Debug.toString grid ]
                        ]
                )
            |> div [ style "padding" "20px" ]
        , examples
            |> List.map toWords
            |> List.map text
            |> div []
        ]


emptyGrid =
    { days = []
    , hours = Set.empty
    , minutes = Set.empty
    , seconds = Set.empty
    }


type alias Grid =
    { days : List Day
    , hours : Set Int
    , minutes : Set Int
    , seconds : Set Int
    }


toGrid grid s =
    case s of
        Seconds ->
            { grid | seconds = Set.fromList <| List.range 0 59 }

        SecondsFrom from to ->
            -- @TODO handle negative wrap-arounds
            { grid | seconds = Set.fromList <| List.range from to }

        Second n ->
            { grid | seconds = Set.insert n grid.seconds }

        Minutes ->
            { grid | minutes = Set.fromList <| List.range 0 59 }

        Minute n ->
            { grid | minutes = Set.insert n grid.minutes }

        Hour n ->
            { grid | hours = Set.insert n grid.hours }

        DaysFrom from to ->
            { grid | days = grid.days ++ allDaysInRange from to }

        Excluding s_ ->
            let
                x =
                    Debug.log "before exclude" grid

                y =
                    Debug.log "after exclude" excluded

                excluded =
                    excludeGrid grid (toGrid emptyGrid s_)
            in
            excluded

        Group sl ->
            let
                mergeGrids g1 g2 =
                    { g1
                        | days = g1.days ++ g2.days
                        , hours = Set.union g1.hours g2.hours
                        , minutes = Set.union g1.minutes g2.minutes
                        , seconds = Set.union g1.seconds g2.seconds
                    }
            in
            sl
                |> List.map (toGrid emptyGrid)
                |> List.foldl mergeGrids emptyGrid

        _ ->
            grid


excludeGrid : Grid -> Grid -> Grid
excludeGrid grid exclude =
    let
        days =
            List.filter (\v -> not <| List.member v exclude.days) grid.days

        x =
            Debug.log "days" days
    in
    { grid | days = days }



-- { days = List.filter (\v -> not <| List.member v exclude.days)
-- , hours = Set.remove (\v -> not <| Set.member v exclude.hours)
-- , minutes = Set.remove (\v -> not <| Set.member v exclude.minutes)
-- , seconds = Set.remove (\v -> not <| Set.member v exclude.seconds)
-- }


allDaysInRange from to =
    List.range (dayToInt from) (dayToInt to)
        |> List.map intToDay


gridWords grid =
    if List.isEmpty grid.days then
        if Set.isEmpty grid.hours then
            if Set.isEmpty grid.minutes then
                if Set.isEmpty grid.seconds then
                    "❌ never runs"

                else
                    showSeconds grid.seconds

            else
                showMinutes grid.minutes

        else
            "daily at " ++ setListAnd grid.hours

    else
        "on " ++ dayListAnd grid.days ++ " at 00:00"


showSeconds seconds =
    if Set.size seconds == 60 then
        "every second"

    else
        "every minute at " ++ setListAnd seconds ++ " seconds"


showMinutes minutes =
    if Set.size minutes == 60 then
        "every minute"

    else
        "hourly at " ++ setListAnd minutes ++ " minutes"


setListAnd s =
    Set.toList s |> intListAnd


listPresent =
    List.isEmpty >> not


intListAnd l =
    case l of
        [] ->
            ""

        i :: [] ->
            String.fromInt i

        l_ ->
            l_
                |> List.map String.fromInt
                |> String.join ", "


dayListAnd days =
    case days of
        [] ->
            ""

        d :: [] ->
            dayToString d

        d_ ->
            d_
                |> List.map dayToString
                |> String.join ", "



-- "❌"


toWords s =
    case s of
        Hour n ->
            "at " ++ String.fromInt n

        _ ->
            "❌"



-- Schedule


examples =
    [ Seconds
    , SecondsFrom 1 15
    , Second 15
    , Minutes
    , Minute 15
    , DaysFrom Mon Fri

    -- General Syntax Rules:
    -- * Expression uses a similar syntax as function calls in C-Style languages: name(arg0, arg1, arg2). The commas between arguments are optional.
    -- * Format strings are case-insensitive. dayofmonth is equivalent to DAYOFMONTH or dayOfMonth, etc.
    -- * All whitespace is insignificant.
    -- * Fractional numbers are not supported in any expression.
    -- * An argument preceded by a ! is treated as an exclude. days(!sat..sun) means that Saturday through Sunday are excluded from the schedule.
    -- * All expressions accept any number of arguments, and may mix includes and excludes. For example, you might specify every weekday except tuesday as days(mon..fri, !tues).
    --
    , Group [ DaysFrom Mon Fri, Excluding (Day Tue) ]

    -- * All time-related values are evaluated as UTC. hours(12) will be noon UTC, not noon local.
    , Hour 12

    -- * Numeric ranges are specified in the form of start..end. For example, days(1..5) is the first five days of the month. The order of start and end is significant, and in cases where start > end it will be interpreted as a range which wraps. In other words, minutes(58..2) means it will run on minutes 58, 59, 0, 1, and 2.
    , MinutesFrom 58 2

    -- * The .. operator is inclusive of the start and end values. 1..4 is equal to 1,2,3,4. There is also a ..< operator which is the "half-open" range operator, meaning it is inclusive of the start, but exclusive of the end value. 1..<4 is equal to 1,2,3.
    -- * The wildcard * operator means "any value." For example, min(*) means "run every minute."
    , Minutes

    -- * The % operator can be used to define intervals. seconds(*%2) will run on all even seconds. seconds(7%3) will run at 7,10,13, ...,55, and 58 seconds of every minute. seconds(7..19%4) will run at 7,11,15, and 19 seconds. seconds(57..4%2) will run at 57,59,1, and 3 seconds. Note that the interval operation is always relative to the start value in the range.
    , Every 2 Seconds
    , Every 3 (SecondsFrom 7 0)
    , Every 4 (SecondsFrom 7 19)

    -- Here are some examples which illustrate these defaults:
    -- * minutes(10) will run at ten minutes after the top of every hour on every day.
    , Minute 10

    -- * hours(12) will run at noon UTC everyday.
    , Hour 12
    , Midday

    -- * daysOfWeek(mon..fri) will run at midnight UTC Mondays through Fridays.
    , DaysFrom Mon Fri

    -- * daysOfWeek(mon) hours(12) will run at noon UTC on Mondays.
    , Day Mon
    , Hour 12

    -- * daysOfWeek(mon) minutes(0, 30) will run at the top and half of every hour on Mondays.
    , Day Mon
    , Minute 0
    , Minute 30

    -- * daysOfYear(*) will run at midnight (00:00:00) every day.
    , DaysOfYear

    -- Examples:
    -- * {hours(10), days(!sat..sun)} {hours(12), days(sat..sun)} Runs 10:00 on weekdays, and noon on weekends.
    , Group [ Group [ Hour 10, DaysFrom Mon Fri ], Group [ Midday, DaysFrom Sat Sun ] ]

    -- * {dates(10/1 .. 3/31) hours(12)} {dates(4/1 .. 9/30) hours(14)} Runs 12:00 during October through March, and at 14:00 during April through September.
    , Group [ Group [ MonthsFrom Oct Mar, Midday ], Group [ MonthsFrom Apr Sep, Hour 14 ] ]
    ]
