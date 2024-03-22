module T15630 where

{- This is a fansastic test cose.

* It scales really easily (just add or remove fields).

* It can demonstrate massive (exponental) blow up if you get inlining
  for join points wrong.

* I found that a more monomorphic variant, T15630a, tickled a very similar
  exponential -blowup, but somehow in a slighlty different way.  To be specific,
  at the time of writing, HEAD was fine on T15630, but blew up on T15630a.
  So both tests are valuable.

* Also worth noting: even if it doesn't blow up, it can result in two
  very different programs.  Below are the good and bad versions for 5
  fields.  Note that the good version passes Maybes to the join points,
  the ultimate values of the fields.  But the bad version passes an
  accumulating *function* to the join points. Lots of PAPs much less
  efficient.

See Note [Do not add unfoldings to join points at birth] in
GHc.Core.Opt.Simplify.Iteration.
-}

data IValue = IDefault
            | IInt Int
            | IBlob String

(?) :: Applicative m => (IValue -> m a) -> IValue -> m (Maybe a)
(?) _ IDefault = pure Nothing
(?) p x = Just <$> p x

getInt :: IValue -> Either () Int
getInt (IInt i) = Right i
getInt v = Left ()

getString :: IValue -> Either () String
getString (IBlob b) = Right $ b
getString v = Left ()

(<+>) :: Applicative m => (m (a -> b), [IValue]) -> (IValue -> m a) -> (m b, [IValue])
(<+>) (f, (v:vs)) p = (f <*> (p v), vs)

data TestStructure = TestStructure
    { _param1 :: Int
    , _param2 :: Maybe String
    , _param3 :: Maybe Int
    , _param4 :: Maybe String
    , _param5 :: Maybe Int
    , _param6 :: Maybe Int
    , _param7 :: Maybe String
    , _param8 :: Maybe String
    , _param9 :: Maybe Int
    , _param10 :: Maybe Int
    , _param11 :: Maybe String
    , _param12 :: Maybe String
    , _param13 :: Maybe Int
    , _param14 :: Maybe Int
    , _param15 :: Maybe String
    }

getMenuItem :: [IValue] -> Either () TestStructure
getMenuItem vs = fst $ (pure TestStructure, vs)
             <+> getInt
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getInt ?)
             <+> (getString ?)
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getInt ?)
             <+> (getString ?)
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getInt ?)
             <+> (getString ?)


{-
------------- The good version (5 fields) ----------------
getMenuItem
  = \ (vs_az6 :: [IValue]) ->
      case vs_az6 of {
        [] -> case T15630.<+>1 of wild1_00 { };
        : v_az3 vs1_az4 ->
          case vs1_az4 of {
            [] -> case T15630.<+>1 of wild2_00 { };
            : v1_X4 vs2_X5 ->
              case vs2_X5 of {
                [] -> case T15630.<+>1 of wild3_00 { };
                : v2_X7 vs3_X8 ->
                  case vs3_X8 of {
                    [] -> case T15630.<+>1 of wild4_00 { };
                    : v3_Xa vs4_Xb ->
                      case vs4_Xb of {
                        [] -> case T15630.<+>1 of wild5_00 { };
                        : v4_Xd vs5_Xe ->
                          case v_az3 of {
                            __DEFAULT -> T15630.getMenuItem1;
                            IInt i_ayQ ->
                              join {
                                $j_sPO [Dmd=MC(1,L)] :: Maybe String -> Either () TestStructure
                                [LclId[JoinId(1)(Nothing)], Arity=1, Str=<L>, Unf=OtherCon []]
                                $j_sPO (y_Xf [OS=OneShot] :: Maybe String)
                                  = join {
                                      $j1_sPR [Dmd=MC(1,L)] :: Maybe Int -> Either () TestStructure
                                      [LclId[JoinId(1)(Nothing)], Arity=1, Str=<L>, Unf=OtherCon []]
                                      $j1_sPR (y1_Xg [OS=OneShot] :: Maybe Int)
                                        = case v3_Xa of {
                                            IDefault ->
                                              case v4_Xd of {
                                                IDefault ->
                                                  Data.Either.Right
                                                    @()
                                                    @TestStructure
                                                    (T15630.TestStructure
                                                       i_ayQ
                                                       y_Xf
                                                       y1_Xg
                                                       (Nothing @String)
                                                       (Nothing @Int));
                                                IInt i1_Xk ->
                                                  Data.Either.Right
                                                    @()
                                                    @TestStructure
                                                    (T15630.TestStructure
                                                       i_ayQ
                                                       y_Xf
                                                       y1_Xg
                                                       (Nothing @String)
                                                       (Just @Int i1_Xk));
                                                IBlob ipv_sPo -> T15630.getMenuItem1
                                              };
                                            IInt ipv_sPm -> T15630.getMenuItem1;
                                            IBlob b_ayW ->
                                              case v4_Xd of {
                                                IDefault ->
                                                  Data.Either.Right
                                                    @()
                                                    @TestStructure
                                                    (T15630.TestStructure
                                                       i_ayQ
                                                       y_Xf
                                                       y1_Xg
                                                       (Just @String b_ayW)
                                                       (Nothing @Int));
                                                IInt i1_Xk ->
                                                  Data.Either.Right
                                                    @()
                                                    @TestStructure
                                                    (T15630.TestStructure
                                                       i_ayQ
                                                       y_Xf
                                                       y1_Xg
                                                       (Just @String b_ayW)
                                                       (Just @Int i1_Xk));
                                                IBlob ipv_sPo -> T15630.getMenuItem1
                                              }
                                          } } in
                                    case v2_X7 of {
                                      IDefault -> jump $j1_sPR (Nothing @Int);
                                      IInt i1_Xi -> jump $j1_sPR (Just @Int i1_Xi);
                                      IBlob ipv_sPk -> T15630.getMenuItem1
                                    } } in
                              case v1_X4 of {
                                IDefault -> jump $j_sPO (Nothing @String);
                                IInt ipv_sPi -> T15630.getMenuItem1;
                                IBlob b_ayW -> jump $j_sPO (Just @String b_ayW)
                              }}}}}}}


------------- The bad version ----------------
getMenuItem
  = \ (vs_azD :: [IValue]) ->
      case vs_azD of {
        [] -> case T15630.<+>1 of wild1_00 { };
        : v_azA vs1_azB ->
          case vs1_azB of {
            [] -> case T15630.<+>1 of wild2_00 { };
            : v1_X5 vs2_X6 ->
              case vs2_X6 of {
                [] -> case T15630.<+>1 of wild3_00 { };
                : v2_X9 vs3_Xa ->
                  case vs3_Xa of {
                    [] -> case T15630.<+>1 of wild4_00 { };
                    : v3_Xd vs4_Xe ->
                      case vs4_Xe of {
                        [] -> case T15630.<+>1 of wild5_00 { };
                        : v4_Xh vs5_Xi ->
                          case v_azA of {
                            __DEFAULT -> T15630.getMenuItem1;
                            IInt i_azn ->
                              join {
                                $j_sQw [Dmd=MC(1,L)]
                                  :: (Maybe String -> Maybe Int -> TestStructure)
                                     -> Either () TestStructure
                                [LclId[JoinId(1)(Nothing)],
                                 Arity=1,
                                 Str=<MC(1,C(1,L))>,
                                 Unf=OtherCon []]
                                $j_sQw (f_aPr [OS=OneShot]
                                          :: Maybe String -> Maybe Int -> TestStructure)
                                  = case v3_Xd of {
                                      IDefault ->
                                        case v4_Xh of {
                                          IDefault ->
                                            Data.Either.Right
                                              @()
                                              @TestStructure
                                              (f_aPr
                                                 (Nothing @String)
                                                 (Nothing @Int));
                                          IInt i1_Xl ->
                                            Data.Either.Right
                                              @()
                                              @TestStructure
                                              (f_aPr
                                                 (Nothing @String)
                                                 (Just @Int i1_Xl));
                                          IBlob ipv_sPM -> T15630.getMenuItem1
                                        };
                                      IInt ipv_sPK -> T15630.getMenuItem1;
                                      IBlob b_azt ->
                                        case v4_Xh of {
                                          IDefault ->
                                            Data.Either.Right
                                              @()
                                              @TestStructure
                                              (f_aPr
                                                 (Just @String b_azt)
                                                 (Nothing @Int));
                                          IInt i1_Xl ->
                                            Data.Either.Right
                                              @()
                                              @TestStructure
                                              (f_aPr
                                                 (Just @String b_azt)
                                                 (Just @Int i1_Xl));
                                          IBlob ipv_sPM -> T15630.getMenuItem1
                                        }
                                    } } in
                              case v1_X5 of {
                                IDefault ->
                                  case v2_X9 of {
                                    IDefault ->
                                      jump $j_sQw
                                        (\ (ds_dNN [OS=OneShot] :: Maybe String)
                                           (ds1_dNO [OS=OneShot] :: Maybe Int) ->
                                           T15630.TestStructure
                                             i_azn
                                             (Nothing @String)
                                             (Nothing @Int)
                                             ds_dNN
                                             ds1_dNO);
                                    IInt i1_Xk ->
                                      jump $j_sQw
                                        (\ (ds_dNN [OS=OneShot] :: Maybe String)
                                           (ds1_dNO [OS=OneShot] :: Maybe Int) ->
                                           T15630.TestStructure
                                             i_azn
                                             (Nothing @String)
                                             (Just @Int i1_Xk)
                                             ds_dNN
                                             ds1_dNO);
                                    IBlob ipv_sPI -> T15630.getMenuItem1
                                  };
                                IInt ipv_sPG -> T15630.getMenuItem1;
                                IBlob b_azt ->
                                  case v2_X9 of {
                                    IDefault ->
                                      jump $j_sQw
                                        (\ (ds_Xl [OS=OneShot] :: Maybe String)
                                           (ds1_Xm [OS=OneShot] :: Maybe Int) ->
                                           T15630.TestStructure
                                             i_azn
                                             (Just @String b_azt)
                                             (Nothing @Int)
                                             ds_Xl
                                             ds1_Xm);
                                    IInt i1_Xk ->
                                      jump $j_sQw
                                        (\ (ds_Xm [OS=OneShot] :: Maybe String)
                                           (ds1_Xn [OS=OneShot] :: Maybe Int) ->
                                           T15630.TestStructure
                                             i_azn
                                             (Just @String b_azt)
                                             (Just @Int i1_Xk)
                                             ds_Xm
                                             ds1_Xn);
                                    IBlob ipv_sPI -> T15630.getMenuItem1
    }}}}}}}}

-}
