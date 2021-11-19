module P4 where
import P2

instance Show Root where
  show None = "no roots"
  show (One root) = show root
  show (Two root1 root2) = show root1 <> " " <> show root2

instance Eq Root where
  None == None = True
  One r1 == One r2 = r1 == r2
  Two r11 r12 == Two r21 r22 = r11 == r22 && r12 == r21 || r11 == r21 && r12 == r22
  x == y = False