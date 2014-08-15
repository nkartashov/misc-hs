{--

From the American Red Cross.

There are four major blood groups determined by the presence or absence of 
two antigens – A and B – on the surface of red blood cells:

Group A – has only the A antigen on red cells (and B antibody in the plasma)
Group B – has only the B antigen on red cells (and A antibody in the plasma)
Group AB – has both A and B antigens on red cells (but neither A nor B antibody 
in the plasma)
Group O – has neither A nor B antigens on red cells (but both A and B antibody 
are in the plasma)
There are very specific ways in which blood types must be matched for a safe 
transfusion. See the chart below: 

[ed: chart translated into words:]

* Group O can donate red blood cells to anybody. It's the universal donor.
* Group A can donate red blood cells to A's and AB's.
* Group B can donate red blood cells to B's and AB's.
* Group AB can donate to other AB's, but can receive from all others.

In addition to the A and B antigens, there is a third antigen called the 
Rh factor, which can be either present (+) or absent ( – ). In general, 
Rh negative blood is given to Rh-negative patients, and Rh positive blood 
or Rh negative blood may be given to Rh positive patients.

The universal red cell donor has Type O negative blood type.
The universal plasma donor has Type AB positive blood type.

Extracted from:

http://www.redcrossblood.org/learn-about-blood/blood-types

Thursday, August 7th, 2014

So with the blood-type data type and the Rh-factor data type, determine
if it is okay for a certain blood type to be the donor to a person (whose
blood type is determined). That's one side: donor.

On the other side, determined if it is safe to be a recipient of a 
certain blood type, given the recipient's blood type.

 --}

data RhFactor = Positive | Negative deriving Eq
data BloodType = O | A | B | AB deriving Eq

safeDonorTo :: (BloodType, RhFactor) -> (BloodType, RhFactor) -> Bool
(bx, rx) `safeDonorTo` (by, ry) = (bx `safeDonorToBlood` by) && (rx `safeDonorToRh` ry)

canReceiveFrom :: (BloodType, RhFactor) -> (BloodType, RhFactor) -> Bool
canReceiveFrom = flip safeDonorTo

safeDonorToBlood :: BloodType -> BloodType -> Bool
O `safeDonorToBlood` _ = True
_ `safeDonorToBlood` AB = True
x `safeDonorToBlood` y = x == y

canReceiveFromBlood :: BloodType -> BloodType -> Bool
canReceiveFromBlood = flip safeDonorToBlood

safeDonorToRh :: RhFactor -> RhFactor -> Bool
_ `safeDonorToRh` Positive = True
x `safeDonorToRh` y = x == y

canReceiveFromRh :: RhFactor -> RhFactor -> Bool
canReceiveFromRh = flip safeDonorToRh


{--

Congratulations! You're an expert blood... person ... thingie.

Legal disclaimer: This site does not represent expertise in ... blah-di-blah-
di-blah. Consult your physician before you do anything stupid, and then,
don't do that stupid thing, 'cause that's just wrong.

 --}
