interface Result
    exposes [
        map2,
    ]
    imports []

map2 : Result ok1 e, Result ok2 e, (ok1, ok2 -> ok) -> Result ok e
map2 = \result1, result2, fn ->
    when result1 is
        Ok ok1 ->
            when result2 is
                Ok ok2 ->
                    Ok (fn ok1 ok2)
                Err e ->
                    Err e
        Err e ->
            Err e
