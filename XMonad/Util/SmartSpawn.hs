module XMonad.Util.SmartSpawn (smartSpawn) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import System.IO.Error (catchIOError)
import System.Directory
import System.Posix.Files
import System.Posix.Types

import Text.Printf

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as SS

getFocusedWindow :: XState -> Maybe Window
getFocusedWindow xstate = do
	windowStack <- SS.stack . SS.workspace . SS.current $ windowset xstate
	return $ SS.focus windowStack

smartSpawn :: String -> X ()
smartSpawn cmd = void . runMaybeT $
	smartSpawnMaybe cmd `mplus` lift (spawn cmd)

smartSpawnMaybe :: String -> MaybeT X ()
smartSpawnMaybe cmd = do
	wnd <- MaybeT $ gets getFocusedWindow 
	pid <- MaybeT . io $ wndPid wnd
	cwd <- liftIO . readSymbolicLink . printf "/proc/%d/cwd" $ toInteger pid
	lift . spawn $ printf "cd %s; %s" cwd cmd
--	--wnd <- gets $ getFocusedWindow
--	--pid <- fmap (io . wndPid) wnd
--	case pid of
--		Nothing -> spawn cmd
--		Just p -> liftIO $ do
--			cwd <- readSymbolicLink . printf "/proc/%d/cwd" $ toInteger p
--			spawn $ printf "cd %s; %s" cwd cmd
	where
		wndPid w = do
			ds <- getDirectoryContents "/proc"
			let ps = map fst . filter ((== "") . snd) . map head . filter (/= []) $ map (reads :: String -> [(ProcessID, String)]) ds
			fmap head' $ filterM (fmap (maybe False (== w)) . winid) ps
			where
				winid pid = do
					s <- flip catchIOError (const $ return "") $
						readFile $ "/proc/" ++ show pid ++ "/environ"
					return . fmap ((read::String->Window) . snd) . head' . filter ((== "WINDOWID") . fst) $ parseEnv s

		parseEnv [] = []
		parseEnv str = concat [(s3, s4) : parseEnv s2 | (s1,s2) <- f $ span (/= '\0') str, (s3,s4) <- f $ span (/= '=') s1]
			where
				f (_,"") = []
				f (a,x:xs) = [(a,xs)]

		head' [] = Nothing
		head' (x:_) = Just x


