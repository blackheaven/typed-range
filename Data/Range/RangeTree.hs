-- TODO I want to come up with a really efficient wap of performing these calculations. To
-- that extent I want to be able to generate a full parse tree that can just be evaluated.
-- That way you only have to convert to the internal representation once and convert out
-- once.

data RangeOperation = RangeUnion | RangeIntersection

data RangeTree a 
   = RangeTreeNode RangeOperation (RangeTree a) (RangeTree a)
   | LeafNode Bool [Range a]
