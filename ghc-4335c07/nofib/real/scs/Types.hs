module Types where
import Data.Array

type Name		= String
type Seed		= Integer
type Time		= Exact
type Temperature	= Approx
type RBC		= Approx

type State		= Vector Approx
type Output		= Vector Approx
type Random		= [Approx]

type Circuit		= [(Index, Index, Index, Element)]
data Element		= Conductor Exact
			| Resistor  Exact
			| Capacitor (Exact, Exact)
			| Inductor  (Exact, Exact)
			| Vsource   List
			| Isource   List
			| Junction  (Exact, Exact, Exact)	deriving Show
type List		= [(Approx, Approx)]

type Index		= Int
type Vector a		= Array  Index         a
type Matrix a		= Array (Index, Index) a

type Exact		= Rational
type Approx		= Double

circa :: Exact -> Approx
circa 			= fromRational
