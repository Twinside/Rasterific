{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module handling math regarding the handling of quadratic
-- and cubic bezier curve.
module Graphics.Rasterific.QuadraticBezier
    ( -- * Bezier representation (types)
      Bezier( .. )
      -- * Helper functions
    , straightLine
    , bezierFromPath
    ) where

import Control.Applicative( (<$>)
                          , (<*>)
                          , Applicative
                          , pure )
import Linear( V2( .. )
             , V1( .. )
             , (^-^)
             , (^+^)
             , (^*)
             , dot
             , norm
             )
import Data.Monoid( Monoid( mempty ), (<>) )
import Data.Foldable( Foldable, foldMap )
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Types

{-import Debug.Trace-}
{-import Text.Printf-}

-- | Type representing a quadratic bezier curve.
data Bezier = Bezier !Point !Point !Point
  deriving (Eq, Show)

-- | Create a list of bezier patch from a list of points,
--
-- > bezierFromPath [a, b, c, d, e] == [Bezier a b c, Bezier c d e]
-- > bezierFromPath [a, b, c, d, e, f] == [Bezier a b c, Bezier c d e]
-- > bezierFromPath [a, b, c, d, e, f, g] ==
-- >     [Bezier a b c, Bezier c d e, Bezier e f g]
--
bezierFromPath :: [Point] -> [Bezier]
bezierFromPath (a:b:rest@(c:_)) = Bezier a b c : bezierFromPath rest
bezierFromPath _ = []

instance Strokable Bezier where
    type RenderType Bezier = Bezier
    strokize = strokizeBezierPath 

instance Rasterizable Bezier where
  decompose = decomposeBeziers
  clip = clipBezier

decomposeBeziers :: Bezier -> [EdgeSample]
decomposeBeziers (Bezier a@(V2 ax ay) b c@(V2 cx cy))
    | insideX && insideY = [EdgeSample (px + 0.5) (py + 0.5) (w * h) h]
    | otherwise = recurse (Bezier m bc c) <> recurse (Bezier a ab m)
  where floorA = vfloor a
        floorC = vfloor c
        V2 px py  = fromIntegral <$> vmin floorA floorC
        V1 w = (px + 1 -) <$>  (V1 cx `midPoint` V1 ax)
        h = cy - ay

        recurse = decomposeBeziers

        V2 insideX insideY =
            floorA ^==^ floorC ^||^ vceil a ^==^ vceil c

        ab = a `midPoint` b
        bc = b `midPoint` c
        abbc = ab `midPoint` bc

        mini = fromIntegral <$> vfloor abbc
        maxi = fromIntegral <$> vceil abbc
        nearmin = vabs (abbc ^-^ mini) ^< 0.1
        nearmax = vabs (abbc ^-^ maxi) ^< 0.1

        minMaxing mi nearmi ma nearma p
          | nearmi = mi
          | nearma = ma
          | otherwise = p

        m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax <*> abbc

-- | Reverse the order of a bezier curve
-- law:
--
-- > reverseBezier (reverseBezier a) = a
--
reverseBezier :: Bezier -> Bezier
reverseBezier (Bezier a b c) = Bezier c b a

-- | Create a quadratic bezier curve representing
-- a straight line.
straightLine :: Point -> Point -> Bezier
straightLine a c = Bezier a (a `midPoint` c) c

-- | Clamp the bezier curve inside a rectangle
-- given in parameter.
clipBezier :: (Applicative a, Monoid (a Bezier))
           => Point     -- ^ Point representing the "minimal" point for cliping
           -> Point     -- ^ Point representing the "maximal" point for cliping
           -> Bezier    -- ^ The quadratic bezier curve to be clamped
           -> a Bezier
clipBezier mini maxi bezier@(Bezier a b c)
    -- If we are in the range bound, return the curve
    -- unaltered
    | insideX && insideY = pure bezier
    -- If one of the component is outside, clamp
    -- the components on the boundaries and output a
    -- straight line on this boundary. Useful for the
    -- filing case, to clamp the polygon drawing on
    -- the edge
    | outsideX || outsideY =
        pure $ clampedA `straightLine` clampedC
    -- Not completly inside nor outside, just divide
    -- and conquer.
    | otherwise =
        recurse (Bezier m bc c) <> recurse (Bezier a ab m)
  where -- Minimal & maximal dimension of the bezier curve
        bmin = vmin a $ vmin b c
        bmax = vmax a $ vmax b c

        recurse = clipBezier mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedC = clamper c

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        --
        --         X B
        --        / \
        --       /   \
        --   ab X--X--X bc
        --     / abbc  \
        --    /         \
        -- A X           X C
        --
        ab = a `midPoint` b
        bc = b `midPoint` c
        abbc = ab `midPoint` bc

        --  mini
        --     +-------------+
        --     |             |
        --     |             |
        --     |             |
        --     +-------------+
        --                   maxi
        -- the edgeSeparator vector encode which edge
        -- is te nearest to the midpoint.
        -- if True then it's the 'min' edges which are
        -- the nearest, otherwise it's the maximum edge
        edgeSeparator =
            vabs (abbc ^-^ mini) ^<^ vabs (abbc ^-^ maxi)

        -- So here we 'solidify' the nearest edge position
        -- in an edge vector.
        edge = vpartition edgeSeparator mini maxi

        -- If we're near an edge, snap the component to the
        -- edge.
        m = vpartition (vabs (abbc ^-^ edge) ^< 0.1) edge abbc

-- | Join two quadratic bezier curves together
joinBeziers :: (Applicative a, Monoid (a Bezier))
            => StrokeWidth -> Join -> Bezier -> Bezier -> a Bezier
joinBeziers offset join (Bezier _ ib ic) (Bezier ja jb _) =
  case join of
    JoinRound -> roundJoin offset ic u v
    JoinMiter l -> miterJoin offset l ic u v
  where u = ib `normal` ic
        v = ja `normal` jb

-- | Put a cap at the end of a bezier curve, depending
-- on the kind of cap wanted.
capBezier :: (Applicative a, Monoid (a Bezier))
          => Float -> Cap -> Bezier -> a Bezier
capBezier offset CapRound (Bezier _ b c) = 
  roundJoin offset c u (- u) where u = b `normal` c
capBezier offset (CapStraight cVal) (Bezier _ b c) = 
   pure (d `straightLine` e) <> pure (e `straightLine` f)
                             <> pure (f `straightLine` g)
  where -- The usual "normal"
        u@(V2 ux uy) = b `normal` c
        -- Vector pointing in the direction of the curve
        -- of norm 1
        v = V2 uy $ negate ux

        -- Finishing points around the edge
        -- -u*offset u*offset
        --       <-><->
        --     d/  /  /g
        --     /  /  /
        --    /  /  /
        --      /
        --     / curve
        --
        d = c ^+^ u ^* offset
        g = c ^-^ u ^* offset

        -- Create the "far" points
        -- 
        --       e        f
        --        /     /   ^
        --       /     /   / v * offset * cVal
        --     d/  /  /g
        --     /  /  /
        --    /  /  /
        --      /
        --     / curve
        --
        e = d ^+^ v ^* (offset * cVal)
        f = g ^+^ v ^* (offset * cVal)


miterJoin :: (Applicative a, Monoid (a Bezier))
          => Float -> Float -> Point -> Vector -> Vector -> a Bezier
miterJoin offset l point u v
  | u `dot` w >= l / max 1 l =
      pure (m `straightLine` c) <> pure (a `straightLine` m)
  -- A simple straight junction
  | otherwise = pure $ a `straightLine` c
  where --      X m
        --     /\   
        --    /|w\
        -- a X---X c
        --    \ /
        --     Xp
        -- ^  / \  ^
        -- u\/   \/v
        --  /     \
        a = point ^+^ u ^* offset
        c = point ^+^ v ^* offset
        w = (a `normal` c) `ifZero` u

        -- Calculate the maximum distance on the
        -- u axis
        p = offset / (u `dot` w)
        -- middle point for "straight joining"
        m = point + w ^* p

-- | Create a "rounded" join or cap
roundJoin :: (Applicative a, Monoid (a Bezier))
          => Float -> Point -> Vector -> Vector -> a Bezier
roundJoin offset p = go
  where go u v
          -- If we're already on a nice curvature,
          -- don't bother doing anything
          | u `dot` w >= 0.9 = pure $ Bezier a b c
          | otherwise = go w v <> go u w
          where --     ^    
                --     |w
                -- a X---X c
                --    \ /
                --     Xp
                -- ^  / \  ^
                -- u\/   \/v
                --  /     \
                a = p ^+^ u ^* offset
                c = p ^+^ v ^* offset

                w = (a `normal` c) `ifZero` u

                -- Same as offseting
                n = p ^+^ w ^* offset
                b = n ^* 2 ^-^ (a `midPoint` c)

offsetAndJoin :: Float -> Join -> Cap -> [Bezier]
              -> [Bezier]
offsetAndJoin _ _ _ [] = []
offsetAndJoin offset join  caping (firstBezier@(Bezier la _ _):rest) =
    go firstBezier rest
  where joiner = joinBeziers offset join
        offseter = offsetBezier offset
        caper = capBezier offset caping

        go prev@(Bezier _ _ cp) []
           | la == cp = joiner prev firstBezier <> offseter prev
           | otherwise = caper prev <> offseter prev
        go prev (x:xs) =
            go x xs <> joiner prev x <> offseter prev

-- | Rewrite the bezier curve to avoid degenerate cases.
sanitizeBezier :: (Applicative a, Monoid (a Bezier))
               => Bezier -> a Bezier
sanitizeBezier bezier@(Bezier a b c)
   -- If the two normals vector are far apart (cos nearly -1)
   --
   --       u           v
   -- <----------   ------------>
   -- because u dot v = ||u|| * ||v|| * cos(uv)
   --
   -- This imply that AB and BC are nearly parallel
   | u `dot` v < -0.9999 =
     -- divide in to halves with
     sanitizeBezier (Bezier abbc (abbc `midPoint` c) c) <>
        sanitizeBezier (Bezier a (a `midPoint` abbc) abbc)

   -- b is far enough of b and c, (it's not a point)
   | norm (a ^-^ b) > 0.0001 && norm (b ^-^ c) > 0.0001 =
       pure bezier

   -- if b is to nearby a or c, take the midpoint as new reference.
   | ac /= b = sanitizeBezier (Bezier a ac c)
   | otherwise = mempty
  where u = a `normal` b
        v = b `normal` c
        ac = a `midPoint` c
        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)

-- | Move the bezier to a new position with an offset.
offsetBezier :: (Applicative a, Monoid (a Bezier))
             => Float -> Bezier -> a Bezier
offsetBezier offset (Bezier a b c)
    -- If the spline is not too curvy, just return the
    -- shifted component
    | u `dot` v >= 0.9 =
        pure $ Bezier shiftedA mergedB shiftedC
    -- Otherwise, divide and conquer
    | a /= b && b /= c =
        offsetBezier offset (Bezier abbc bc c) <>
            offsetBezier offset (Bezier a ab abbc)
    | otherwise = mempty
  where --
        --         X B   
        --    ^   /^\   ^
        --   u \ /w| \ / v
        --      X-----X
        --     /       \
        --    /         \
        -- A X           X C
        --
        u = a `normal` b
        v = b `normal` c
        w = ab `normal` bc

        ab = (a `midPoint` b)
        bc = (b `midPoint` c)
        abbc = ab `midPoint` bc

        shiftedA = a ^+^ (u ^* offset)
        shiftedC = c ^+^ (v ^* offset)
        shiftedABBC = abbc ^+^ (w ^* offset)
        mergedB =
            (shiftedABBC ^* 2.0) ^-^ (shiftedA `midPoint` shiftedC)

-- | Transform a bezier path to a bezier shape ready
-- to be filled
strokizeBezierPath :: StrokeWidth -> Join -> (Cap, Cap) -> [Bezier]
                   -> [Bezier]
strokizeBezierPath width join (capStart, capEnd) beziers =
    offseter capEnd sanitized <> 
        offseter capStart (reverse $ reverseBezier <$> sanitized)
  where sanitized = foldMap sanitizeBezier beziers
        offseter = offsetAndJoin (width / 2) join
