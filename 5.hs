import Data.Maybe
--��� ������ maybe Just � isJust

(~=) x y = abs (x-y) < 0.001

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

treeSize (Leaf _) = 1
treeSize (Branch l r) = treeSize l + treeSize r

leafList (Leaf x)           = [x]
leafList (Branch left right)= leafList left ++ leafList right

data  Expr = Const Integer
            | Add Expr Expr
            | Mult Expr Expr



 --2 �������--

 --�������� ���� 
data BinS = Tip | Bin (String, Int) BinS BinS deriving Show
 
 --������� ������� ���� ����� � ��� ��������
add :: (String, Int) -> BinS -> BinS
add (k, v) Tip = Bin (k, v) Tip Tip
add (k, v)(Bin (k1, v1) l r)
            | k > k1 = Bin (k1, v1) l (add (k, v) r) 
            | otherwise = Bin (k1, v1) (add (k, v) l) r
 
 --������� ������ ������ ������
find :: String -> BinS -> Maybe Int
find k Tip = Nothing 
find k (Bin (k1, v1) l r) 
            | k == k1 = Just v1
            | otherwise = find k (if k > k1 then r else l)
 
 --�������� �� �������������
exist :: String -> BinS -> Bool
exist k = isJust . find k
 
 --�������������� � 1 ��������� ������
toList :: BinS -> [Int]
toList Tip = []
toList( Bin (_, v1) l r) = concat [toList l, [v1], toList r]
