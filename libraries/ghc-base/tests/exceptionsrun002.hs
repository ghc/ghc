module Main where

import qualified Control.Exception as Exception
import System.IO.Error (mkIOError, catchIOError)
import Data.IORef

safeCatch :: IO () -> IO ()
safeCatch f = Exception.catch f
                  ((\_ -> return ()) :: Exception.SomeException -> IO ())

type Thrower = IO Bool

type Catcher = IO Bool -> IO () -> IO ()

checkCatch :: Catcher -> Thrower -> IO Bool
checkCatch catcher thrower = do
    ref <- newIORef False
    safeCatch (catcher thrower (writeIORef ref True))
    readIORef ref

data Named a = MkNamed String a

checkNamedCatch :: Named Catcher -> Named Thrower -> IO ()
checkNamedCatch (MkNamed cname catcher) (MkNamed tname thrower) = do
    didCatch <- checkCatch catcher thrower
    putStrLn (cname ++ (if didCatch then " CAUGHT " else " MISSED ") ++ tname)

checkNamedCatches :: [Named Catcher] -> [Named Thrower] -> IO ()
checkNamedCatches []     _      = return ()
checkNamedCatches _      []     = return ()
checkNamedCatches [c]    (t:tr) = do checkNamedCatch c t
                                     checkNamedCatches [c] tr
checkNamedCatches (c:cr) ts     = do checkNamedCatches [c] ts
                                     checkNamedCatches cr ts


-- throwers

returnThrower :: Named Thrower
returnThrower = MkNamed "return" (return True)

returnUndefinedThrower :: Named Thrower
returnUndefinedThrower = MkNamed "return undefined" (return undefined)

returnErrorThrower :: Named Thrower
returnErrorThrower = MkNamed "return error" (return (error "some error"))

undefinedThrower :: Named Thrower
undefinedThrower = MkNamed "undefined" undefined

failThrower :: Named Thrower
failThrower = MkNamed "fail" (fail "some failure")

errorThrower :: Named Thrower
errorThrower = MkNamed "error" (error "some error")

throwThrower :: Named Thrower
throwThrower = MkNamed "Exception.throw"
 (Exception.throw (Exception.ErrorCall "throw error"))

ioErrorErrorCallThrower :: Named Thrower
ioErrorErrorCallThrower = MkNamed "ioError ErrorCall"
 (Exception.throwIO (Exception.ErrorCall "throw error"))

ioErrorIOExceptionThrower :: Named Thrower
ioErrorIOExceptionThrower = MkNamed "ioError IOException"
 (Exception.throwIO (mkIOError undefined undefined undefined undefined))

returnThrowThrower :: Named Thrower
returnThrowThrower = MkNamed "return Exception.throw"
 (return (Exception.throw (Exception.ErrorCall "throw error")))


-- catchers

bindCatcher :: Named Catcher
bindCatcher = MkNamed ">>" (>>)

preludeCatchCatcher :: Named Catcher
preludeCatchCatcher = MkNamed "Prelude.catch"
 (\f cc -> catchIOError (f >> (return ())) (const cc))

ceCatchCatcher :: Named Catcher
ceCatchCatcher = MkNamed "Exception.catch"
 (\f cc -> Exception.catch (f >> (return ())) (const cc :: Exception.SomeException -> IO ()))

finallyCatcher :: Named Catcher
finallyCatcher = MkNamed "Exception.finally"
 (\f cc -> Exception.finally (f >> (return ())) cc)

main = checkNamedCatches
        [bindCatcher,preludeCatchCatcher,ceCatchCatcher,finallyCatcher]
        [returnThrower,returnUndefinedThrower,returnThrowThrower,returnErrorThrower,failThrower,
        errorThrower,throwThrower,ioErrorErrorCallThrower,ioErrorIOExceptionThrower,undefinedThrower]

