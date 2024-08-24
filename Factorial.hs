import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.Batch (defOpt, run, runTests, TestOptions, TestResult)
import qualified Data.Set as Set
import Data.List (partition, transpose, nub, minimumBy, find)
import Data.Ord (comparing)
import Control.Monad (sequence)
-- types --
{-
  - A normal tableau is row-major and has a list of constraints as well as
  candidates paired with their row.
  - A constraint tableau is relative and column-major.
  It pairs constraints with their ERC.
  - A candidate tableau is absolute and column-major.
  It maintains candidates in a separate list.
-}
type Tableau = ([Constraint], [Row])
type CandTableau = ([Candidate], [[Int]]) -- both are column major,
type ConTableau = [Column] -- but ConTableau is a relative tableau

data Erc = L | E | W deriving (Eq, Ord, Show, Read)
type Constraint = String
type Candidate = String
type Column = (Constraint, [Erc])
type Row = (Candidate, [Int])
-- naive --
permute [] = [[]]
permute l = [x:ys | (x,xs) <- extractions l, ys <- permute xs]
naive :: [Tableau] -> [[Candidate]]
naive = nub . transpose . map (map eval . factorial . colify)
    where factorial (cands,cols) = map ((,) cands) . permute $ cols
eval :: CandTableau -> Candidate
eval ([cand],[viol]:cols) = cand -- quit early when there is one winner
eval (cands, col:cols) = eval (promote cands (col,cols))
eval (cands, []) = head cands
colify :: Tableau -> CandTableau
colify (_, rows) = (map fst rows, transpose (map snd rows))
promote :: [Candidate] -> ([Int],[[Int]]) -> CandTableau
promote cands (col,cols) = (filterLosers cands,map filterLosers cols)
    where filterLosers = filterProxy (==minimum col) col
-- Sanders --
-- TODO: a lot of the names in this section are fairly stupid.
bounded :: [Tableau] -> [[Candidate]]
bounded = Set.toList . factStep . map colify
factStep :: [CandTableau] -> Set.Set [Candidate]
factStep tabs | any singleCol tabs = Set.singleton (map promoteLast tabs)
factStep tabs = Set.unions . map recur . step $ tabs
recur :: [CandTableau] -> Set.Set [Candidate]
recur tabs | all singleRow tabs = Set.singleton (map (head . fst) tabs)
           | otherwise = factStep (withCands informative tabs)
promoteLast :: CandTableau -> Candidate
promoteLast (cands,col:cols) = head . fst $ promote cands (col, [])
step :: [CandTableau] -> [[CandTableau]]
step tabs = map (map (uncurry promote) . zip candsets) tabsets
    where candsets = map fst tabs
          tabsets = check . transpose . map (extractions . snd) $ tabs
          check = filter (any (notSame . fst))
informative :: [[[Int]]] -> [[[Int]]]
informative violations = map (filterProxy id informativeCols) violations
    where informativeCols = map (any notSame) (transpose violations)
notSame = any (uncurry (/=)) . win2
singleRow = (==1) . length . head . snd
singleCol = (==1) . length . snd
withCands f tabs = zip (map fst tabs) (f (map snd tabs))
-- Hayes/Riggle --
hayes = exponential rcd
riggle = exponential rVolume
exponential :: (ConTableau -> Bool) -> [Tableau] -> [[Candidate]]
exponential test = map (map fst) . filter succeed . sequence . map winners
    where succeed = test . combine
          winners :: Tableau -> [(Candidate,ConTableau)]
          winners (con,rows) =
              [comparative (row,(con,rows)) | (row, rows) <- extractions rows]
comparative :: (Row,Tableau) -> (Candidate,ConTableau)
comparative ((cand,viols),(con,tab)) =
    (cand, zip con (transpose . map (sub viols) . snd . unzip $ tab))
    where sub row1 row2 = map classify (zipWith (-) row1 row2)
          classify n | n < 0 = W
                     | n > 0 = L
                     | otherwise = E
combine :: [(Candidate,ConTableau)] -> ConTableau
combine extracted =
    zip (map fst cols) (map concat . transpose . map (map snd) $ allcols)
    where allcols@(cols:_) = map snd extracted
-- RCD --
rcd :: ConTableau -> Bool
rcd cols = case partition (any (==L) . snd) cols of
               (_,[]) -> False
               ([],_) -> True
               (demote,promote) -> rcd (demote \*\ promote)
( \*\ ) :: ConTableau -> ConTableau -> ConTableau
demote \*\ promote = alwaysZip titles (transpose filtered)
    where titles = map fst demote
          vlns = transpose (map snd demote)
          filtered = filterProxy (all (==E)) (transpose (map snd promote)) vlns
-- r-volume --
rVolume :: ConTableau -> Bool
rVolume cols = any r (extractions cols)
    where r (col@(_,erc),cols) | any (==L) erc = False
                               | all (==W) erc = True
                               | otherwise = rVolume (cols \\\ col)
(\\\) :: [Column] -> Column -> [Column]
cols \\\ (title,erc) = alwaysZip titles (transpose filtered)
    where (titles,ercs) = unzip cols
          filtered = filterProxy (/=W) erc (transpose ercs)
--- util ---
fact 0 = 1
fact n = n * fact (n-1)
count f = length . filter f
alwaysZip first second = zip first (if second==[] then repeat [] else second)
filterProxy p proxy l = map fst (filter (p . snd) (zip l proxy))
extractions l = extract l []
    where extract [] _ = []
          extract (x:xs) prev = (x, prev++xs) : extract xs (x : prev)
win2 l = zip l (tail l)
--- tests ---
prop_len l = length l == length (extractions l)
prop_lenRest l = all (==length l - 1) (map (length . snd) (extractions l))
prop_restOrder l = and [xs == (reverse (take i l) ++ tail (drop i l))
                        | (i,xs) <- zip [0..] (map snd (extractions l))]
prop_returnOrder l = l == map fst (extractions l)
prop_lists :: [Int] -> Property
-- lists length 7 and up take too long
prop_lists l = length l < 7 ==> and (zipWith (==)
                                      (map Set.fromList (permute l))
                                      (repeat (Set.fromList l)))
-- test_factorial :: [Int] -> Bool
-- test_factorial _ = factorial [1,2,3] == [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,2,1], [3,1,2]]

-- Axininca Campa example from McCarthy's Thematic Guide to OT --
axininca :: ConTableau
axininca = [("Onset", [L,L,W,E,L]),
            ("Max",   [E,W,E,W,W]),
            ("Dep",   [W,E,L,L,E]),
            ("Dep-init",[W,E,E,E,W])]
axinincaOriginal :: Tableau
axinincaOriginal = (["Onset", "Max", "Dep", "Dep-init"],
                    [("iNkomati",  [1,0,1,0]),
                     ("tiNkomati", [0,0,2,1]),
                     ("komati",    [0,2,1,0]),
                     ("iNkomai",   [2,0,0,0]),
                     ("iNkoma",    [1,1,0,0]),
                     ("tiNkoma",   [0,1,1,1])])
                   {-[("Onset", [-1,-1,1,0,-1]),
                    ("Max",   [0,2,0,1,1]),
                    ("Dep",   [1,0,-1,-1,0]),
                    ("Dep-init",[1,0,0,0,1])]-}
cost :: [Tableau]
cost = [(["*Complex", "Onset", "Align-L-W", "Align-R-P", "Parse"],
         [("cost][us", [1,1,0,0,0]),
          ("cos]t[us", [0,1,0,0,1]),
          ("cos][tus", [0,0,1,0,0])]),
        (["*Complex", "Onset", "Align-L-W", "Align-R-P", "Parse"],
         [("cost][me", [1,0,0,0,0]),
          ("cos]t[me", [0,0,0,0,1]),
          ("cos][tme", [1,0,1,0,0])]),
        (["*Complex", "Onset", "Align-L-W", "Align-R-P", "Parse"],
         [("cost]",    [1,0,0,0,0]),
          ("cos]t",    [0,0,0,1,1])])]
countingTest = [("C1", [W,W,W]),
                ("C2", [W,E,W]),
                ("C3", [W,E,E]),
                ("C4", [L,W,E]),
                ("C5", [L,L,L])]
main = runTests "extractions" defOpt [r prop_len,
                                      r prop_lenRest,
                                      r prop_restOrder,
                                      r prop_returnOrder,
                                      run prop_lists]
       where r = run :: ([Int] -> Bool) -> TestOptions -> IO TestResult
