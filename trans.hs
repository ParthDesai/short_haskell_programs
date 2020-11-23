
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (runStateT, get, modify, StateT)

data MyState = MyState {
    start_code :: Int,
    end_code :: Int,
    app_data :: String
} deriving Show

modify_start_code :: (Int -> Int) -> MyState -> MyState
modify_start_code f input = MyState{start_code=f $ start_code input, end_code=end_code input, app_data = app_data input}

type StateReader = ReaderT MyState Maybe 

-- ReaderT Int Maybe MyState
composition :: StateReader MyState
composition = ReaderT (\r -> Just (MyState {start_code=0, end_code=0, app_data="0"})) >> lift (Just (MyState {start_code=56, end_code=87, app_data="Surprise Pikachu"})) >>= (\state ->  (asks end_code >>= (\input_end_code -> if start_code state > input_end_code then lift Nothing else lift $ Just state) >>= (\state -> if app_data state == "Hello" then lift Nothing else lift $ Just state)))

composition2 :: ReaderT MyState Maybe Int
--composition2 = ReaderT (\r -> if start_code r == 0 then Nothing else Just 1)  >>= (\state ->  if end_code state == 0 then lift Nothing else lift $ Just 2) >>= (\state -> if app_data state == "Hello" then lift Nothing else lift $ Just 3)
composition2 = ReaderT (\r -> if start_code r == 0 then Nothing else Just (start_code r))  >>= (\status -> if status == 1 then lift Nothing else lift (Just $ status + 1)) >>= (\status -> if status == 2 then lift Nothing else lift (Just $ status + 1))
 >>= (\status -> (asks end_code) >>= (\end_code -> if end_code == 0 then lift Nothing else lift $ Just $ 100+status+end_code))

type MutableState = StateT MyState Maybe
composition3 :: MutableState Int
composition3 = modify (modify_start_code (*2)) >>= (\_ -> modify (modify_start_code (*4))) >>= (\_ -> get) >>= (\state -> lift $ Just $ end_code state) 


