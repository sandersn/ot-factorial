import Data.List (partition, transpose)
import Control.Monad (sequence)
-- types --
data Rank = L | E | W deriving (Eq, Ord, Show, Read)
type Erc = [Rank]
type Constraint = String
type Candidate = String
type Column = (Constraint, Erc)
type Row = (Candidate, [Int])
type ColTableau = [Column]
type Tableau = ([Constraint], [Row])
type UnzipTableau = ([Candidate], [[Int]])

type Stratum = [Constraint]
type Strata = [Stratum]

addStratum :: ColTableau -> Strata -> Strata
addStratum cols strata = map fst cols : strata
-- Hayes --
hayes = combinatorial fastrcd
combinatorial :: (ColTableau -> Bool) -> [Tableau] -> [[Candidate]]
combinatorial test = map winners . filter succeed . sequence . map extracted
    where extracted :: Tableau -> [(Row,Tableau)]
          extracted (con,rows) =
              [(row,(con,rows)) | (row, rows) <- extractions rows]
          succeed = test . combine . map comparative
          winners = map (fst . fst)
comparative :: (Row,Tableau) -> (Candidate,ColTableau)
comparative ((cand,viols),(con,tab)) =
    (cand, zip con (transpose (map (sub viols) (snd (unzip tab)))))
    where sub row1 row2 = map classify (zipWith (-) row1 row2)
          classify n | n > 0 = L
                     | n < 0 = W
                     | otherwise = E
combine :: [(Candidate,ColTableau)] -> ColTableau
combine extractions =
    zip (map fst cols) (map concat . transpose . map (map snd) $ allcols)
    where allcols@(cols:_) = map snd extractions
-- rcd ~= rVolume ??!
( \*\ ) :: ColTableau -> ColTableau -> ColTableau
demote \*\ promote = alwaysZip titles (transpose filtered)
    where titles = map fst demote
          scores = transpose (map snd demote)
          filtered = filterProxy (all(==E)) (transpose (map snd promote)) scores
rcd :: ColTableau -> Strata
rcd cols = if promote==[] || demote==[]
           then addStratum cols []
           else addStratum promote (rcd (demote \*\ promote))
    where (demote,promote) = partition (any (==L) . snd) cols
fastrcd :: ColTableau -> Bool
fastrcd cols = case partition (any (==L) . snd) cols of
               (_,[]) -> False
               ([],_) -> True
               (demote,promote) -> fastrcd (demote \*\ promote)
slowrcd cols = case partition (any (==L) . snd) cols of
               (_,[]) -> error "RCD failed"
               ([],promote) -> addStratum promote []
               (demote,promote) -> addStratum promote (rcd (demote \*\ promote))
rVolumeLft cols = case partition (any (==L) . snd) cols of
                  (losers,[]) -> 0
                  ([],winners) -> fact (length winners)
                  (winners,losers) -> sum (map (rVolumeLft . (losers \\\ )) winners)
{- this doesn't do what I thought it would because we are looking for winners
 instead of losers maybe. RCD promotes winners and removes their info.
 rVolume does ??? with winners?-}
rRcd cols = case partition (any (==L) . snd) cols of
                  (losers,[]) -> error "rRCD failed" -- [[]]
                  ([],winners) -> addStratum winners []
                  (winners,losers) ->
                    addStratum winners
                               (concatMap (rRcd . (losers \\\ )) winners)
rFactorial :: ColTableau -> [[String]]
rFactorial cols = concatMap r (extractions cols)
    where r (col@(title,erc),cols)
              | any (==L) erc = []
              | all (==W) erc = map (title:) (factorial (map fst cols))
              | otherwise = map (title:) (rFactorial (cols \\\ col))
          cols \\\ (_,erc) = alwaysZip titles (transpose filtered)
              where (titles, ercs) = unzip cols
                    filtered = filterProxy (/=W) erc (transpose ercs)
-- Riggle --
riggle = combinatorial rVolumeBool
rVolumeBool :: ColTableau -> Bool
rVolumeBool cols = any r (extractions cols)
    where r (col@(_,erc),cols) | any (==L) erc = False
                               | all (==W) erc = True
                               | otherwise = rVolumeBool (cols \\\ col)
rVolume :: ColTableau -> Int
rVolume cols = sum (map r (extractions cols))
    where r (col@(_,erc),cols) | any (==L) erc = 0
                               | all (==W) erc = fact (length cols)
                               | otherwise = rVolume (cols \\\ col)
(\\\) :: [Column] -> Column -> [Column]
cols \\\ (title,erc) = alwaysZip titles (transpose filtered)
    where (titles,ercs) = unzip cols
          filtered = filterProxy (/=W) erc (transpose ercs)
{-- these two clauses are only an efficiency boost to avoid some recursion
rVolume [] = 0
rVolume cols | length (snd (head cols)) == 1 = (fact k * ws) `quot` (k - es)
    where erc = snd (head cols)
          k = length cols
          ws = count (==W) erc
          es = count (==E) erc --}
--- util ---
factorial [] = [[]]
factorial l = [x:ys | (x,xs) <- extractions l, ys <- factorial xs]
fact 0 = 1
fact n = n * fact (n-1)
count f = length . filter f
alwaysZip first second = zip first (if second==[] then repeat [] else second)
filterProxy p proxy l = map fst (filter (p . snd) (zip l proxy))
extractions [] = []
extractions l = extract l []
    where extract [] _ = []
          extract (x:xs) prev = (x, prev++xs) : extract xs (x : prev)
