data Tree = Nil | Node Int Tree Tree

data DOrd = Infix | Prefix | Postfix | GspInfix | GspPrefix | GspPostfix

flatten :: Tree -> DOrd -> [Int]
flatten Nil _ = []
flatten (Node x left right) ord = case ord of
    Infix      -> flatten left Infix      ++ [x] ++ flatten right Infix
    Prefix     -> [x] ++ flatten left Prefix     ++ flatten right Prefix
    Postfix    -> flatten left Postfix    ++ flatten right Postfix    ++ [x]
    GspInfix   -> flatten right GspInfix   ++ [x] ++ flatten left GspInfix
    GspPrefix  -> [x] ++ flatten right GspPrefix  ++ flatten left GspPrefix
    GspPostfix -> flatten right GspPostfix ++ flatten left GspPostfix ++ [x]