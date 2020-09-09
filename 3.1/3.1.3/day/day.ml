type day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let next_day d =
    match d with
    | Monday    -> Tuesday
    | Tuesday   -> Wednesday
    | Wednesday -> Thursday
    | Thursday  -> Friday
    | Friday    -> Saturday
    | Saturday  -> Sunday
    | Sunday    -> Monday
