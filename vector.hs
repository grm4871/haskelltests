data Vec3 a = Vec3 a a a deriving (Show, Eq)

-- Vec3 addition
vadd :: (Num t) => Vec3 t -> Vec3 t -> Vec3 t
(Vec3 i j k) `vadd` (Vec3 l m n) = Vec3 (i+l) (j+m) (k+n)

-- Vec3 subtraction
vsub :: (Num t) => Vec3 t -> Vec3 t -> Vec3 t
(Vec3 i j k) `vsub` (Vec3 l m n) = Vec3 (i-l) (j-m) (k-n)

-- Vec3 multiplication
vmult :: (Num t) => t -> Vec3 t -> Vec3 t  
m `vmult` (Vec3 i j k) = Vec3 (i*m) (j*m) (k*m)

-- Vec3 dot product
vdot :: (Num t) => Vec3 t -> Vec3 t -> Vec3 t
(Vec3 i j k) `vdot` (Vec3 l m n) = Vec3 (i*l) (j*m) (k*n)

-- Vec3 magnitude functions
vmagSquared :: (Num t) => Vec3 t -> t
vmagSquared (Vec3 i j k) = (i*i + j*j + k*k)

vmagFloat :: (Floating t) => Vec3 t -> t
vmagFloat (Vec3 i j k) = sqrt (i*i + j*j + k*k)

vmagInt :: (Integral t, Floating u) => Vec3 t -> u 
vmagInt (Vec3 i j k) = sqrt (fromIntegral $ i*i + j*j + k*k)