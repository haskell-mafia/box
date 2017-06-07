{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Box.Data (
    Box (..)
  , BoxStore (..)
  , BoxFile (..)
  , Query (..)
  , Exact (..)
  , Infix (..)
  , InstanceId (..)
  , Host (..)
  , Name (..)
  , Client (..)
  , Flavour (..)
  , GatewayType (..)
  , BoxError (..)
  , Environment (..)
  , HostKey (..)
  , Context (..)
  , BoxStatus (..)
  , TZ (..)
  , ImageId (..)
  , AvailibilityZone (..)
  , Lifecycle (..)
  , Project (..)
  , Note (..)
  , queryHasMatch
  , queryFromText
  , queryParser
  , queryRender
  , queryOfBox
  , boxShortName
  , boxesFromText
  , boxFromText
  , boxesToText
  , boxToText
  , boxParser
  , boxErrorRender
  , selectRandomBox
  , gatewayFlavour
  ) where

import           Data.Attoparsec.Text as AP
import           Data.Text as T

import           Mismi.S3.Data

import           P

import           System.IO
import           System.Random.Shuffle

------------------------------------------------------------------------
-- Types

data Box =
  Box {
      boxClient     :: Client
    , boxFlavour    :: Flavour
    , boxName       :: Name
    , boxInstance   :: InstanceId
    , boxHost       :: Host
    , boxPublicHost :: Host
    , boxContext    :: Context
    , boxStatus     :: BoxStatus
    , boxTimezone   :: TZ
    , boxImageId    :: ImageId
    , boxAvailZone  :: AvailibilityZone
    , boxLifecycle  :: Lifecycle
    , boxHostKey    :: HostKey
    , boxProject    :: Project
    , boxNote       :: Note
    } deriving (Eq, Ord, Show)

data BoxStore = BoxStore {
    unBoxFiles :: [BoxFile]
  } deriving (Eq, Show)

data BoxFile =
    BoxFileLocal FilePath
  | BoxFileS3 Address
  deriving (Eq, Show)

data Query = Query {
    queryClient   :: Exact Client
  , queryFlavour  :: Exact Flavour
  , queryName     :: Infix Name
  , queryInstance :: Exact InstanceId
  } deriving (Eq, Show)

data Exact a =
    Exact a
  | ExactAll
  deriving (Eq, Show)

data Infix a =
    Infix a
  | InfixAll
  deriving (Eq, Show)

newtype InstanceId =
  InstanceId {
    unInstanceId :: Text
  } deriving (Eq, Ord, Show)

newtype Host =
  Host {
    unHost :: Text
  } deriving (Eq, Ord, Show)

newtype Name =
  Name {
    unName :: Text
  } deriving (Eq, Ord, Show)

newtype Client =
  Client {
    unClient :: Text
  } deriving (Eq, Ord, Show)

newtype Flavour =
  Flavour {
    unFlavour :: Text
  } deriving (Eq, Ord, Show)

newtype Context =
  Context {
    unContext :: Text
  } deriving (Eq, Ord, Show)

newtype BoxStatus =
  BoxStatus {
    unBoxStatus :: Text
  } deriving (Eq, Ord, Show)

newtype TZ =
  TZ {
    unTZ :: Text
  } deriving (Eq, Ord, Show)

newtype ImageId =
  ImageId {
    unImageId :: Text
  } deriving (Eq, Ord, Show)

newtype AvailibilityZone =
  AvailibilityZone {
    unAvailibilityZone :: Text
  } deriving (Eq, Ord, Show)

newtype Lifecycle =
  Lifecycle {
    unLifecycle :: Text
  } deriving (Eq, Ord, Show)

newtype Project =
  Project {
    unProject :: Text
  } deriving (Eq, Ord, Show)

newtype Note =
  Note {
    unNote :: Text
  } deriving (Eq, Ord, Show)

data BoxError =
    BoxFileNotFound BoxFile
  | BoxParseError Text
  deriving (Eq, Show)

data GatewayType =
    -- | Uses single-factor pubkey authentication.
    Gateway
    -- | Uses pubkey authentication + TOTP.
  | GatewaySecure
  deriving (Eq, Show)

data Environment =
    SomeEnv Text
  | DefaultEnv
  deriving (Eq, Show)

newtype HostKey =
  HostKey {
    unHostKey :: Text
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Query

isExactMatch :: Exact a -> Bool
isExactMatch (ExactAll) = False
isExactMatch (Exact _)  = True

isInfixMatch :: Infix a -> Bool
isInfixMatch (InfixAll) = False
isInfixMatch (Infix _)  = True

queryHasMatch :: Query -> Bool
queryHasMatch (Query c f n i) =
  isExactMatch c || isExactMatch f || isInfixMatch n || isExactMatch i

queryFromText :: Text -> Either Text Query
queryFromText =
  first T.pack . parseOnly queryParser

queryParser :: Parser Query
queryParser =
  part `AP.sepBy1` delim <* AP.endOfInput >>= \case
    []           -> pure $ Query (mClient Nothing) (mFlavour Nothing) (mName Nothing) (mInstance Nothing)
    (c:[])       -> pure $ Query (mClient c)       (mFlavour Nothing) (mName Nothing) (mInstance Nothing)
    (c:f:[])     -> pure $ Query (mClient c)       (mFlavour f)       (mName Nothing) (mInstance Nothing)
    (c:f:n:[])   -> pure $ Query (mClient c)       (mFlavour f)       (mName n)       (mInstance Nothing)
    (c:f:n:i:[]) -> pure $ Query (mClient c)       (mFlavour f)       (mName n)       (mInstance i)
    _            -> fail "filter can only contain four parts (client, flavour, name, instance)"
  where
    mClient   = maybe ExactAll (Exact . Client)
    mFlavour  = maybe ExactAll (Exact . Flavour)
    mName     = maybe InfixAll (Infix . Name)
    mInstance = maybe ExactAll (Exact . InstanceId)

    part  = takeMaybe <$> AP.takeWhile (/= ':')
    delim = AP.char ':'

    takeMaybe t | T.null t  = Nothing
                | otherwise = Just t

queryRender :: Query -> Text
queryRender (Query c f n i) =
       exactRender unClient     c
    <> sepcf
    <> exactRender unFlavour    f
    <> sepfn
    <> infixRender unName       n
    <> sepni
    <> exactRender unInstanceId i
  where
    -- we need a separator between client/flavour if
    -- flavour, name, or instance are not a wildcard match
    sepcf | im || nm || fm = ":"
          | otherwise      = ""

    -- we need a separator between flavour/name if
    -- name or instance is not a wildcard match
    sepfn | im || nm  = ":"
          | otherwise = ""

    -- we need a separator between name/instance if
    -- instance is not a wildcard match
    sepni | im        = ":"
          | otherwise = ""

    fm = isExactMatch f
    nm = isInfixMatch n
    im = isExactMatch i

queryOfBox :: Box -> Query
queryOfBox b = Query (Exact (boxClient    b))
                     (Exact (boxFlavour   b))
                     (Infix (boxShortName b))
                     (Exact (boxInstance  b))

exactRender :: (a -> Text) -> Exact a -> Text
exactRender _ (ExactAll) = ""
exactRender f (Exact x)  = f x

infixRender :: (a -> Text) -> Infix a -> Text
infixRender _ (InfixAll) = ""
infixRender f (Infix x)  = f x

------------------------------------------------------------------------
-- Box

boxShortName :: Box -> Name
boxShortName b = Name (dropPrefix namePrefix name)
  where
    name       = unName    (boxName    b)
    namePrefix = unClient  (boxClient  b) <> "."
              <> unFlavour (boxFlavour b) <> "."

    dropPrefix p t
      | p `T.isPrefixOf` t = T.drop (T.length p) t
      | otherwise          = t

boxesFromText :: Text -> Either Text [Box]
boxesFromText =
  traverse boxFromText . T.lines

boxFromText :: Text -> Either Text Box
boxFromText =
  first T.pack . parseOnly boxParser

boxesToText :: [Box] -> Text
boxesToText = T.unlines . fmap boxToText

boxToText :: Box -> Text
boxToText (Box (Client c) (Flavour f) (Name n) (InstanceId i) (Host h) (Host p) (Context ctx) (BoxStatus st)
            (TZ tz) (ImageId img) (AvailibilityZone az) (Lifecycle lc) (HostKey hk) (Project pr) (Note nt)) =
  T.intercalate "\t" [i, h, p, n, c, f, st, tz, ctx, img, az, lc, hk, pr, nt]


-- Format specified in 'power' project, file 'src/Power/Box/V3.hs'
boxParser :: Parser Box
boxParser = do
  let t = AP.takeWhile (/= '\t')
      ts = t <* char '\t'
  i <- InstanceId <$> ts
  h <- Host <$> ts
  p <- Host <$> ts
  n <- Name <$> ts
  c <- Client <$> ts
  f <- Flavour <$> ts
  is <- BoxStatus <$> ts
  tz <- TZ <$> ts
  ctx <- Context <$> ts
  im <- ImageId <$> ts
  az <- AvailibilityZone <$> ts
  lc <- Lifecycle <$> ts
  hk <- HostKey <$> ts
  pr <- Project <$> ts
  nt <- Note <$> AP.takeText
  endOfInput
  pure $ Box c f n i h p ctx is tz im az lc hk pr nt

boxFileRender :: BoxFile -> Text
boxFileRender (BoxFileLocal f) =
  T.pack f
boxFileRender (BoxFileS3 a) =
  addressToText a

boxErrorRender :: BoxError -> Text
boxErrorRender (BoxFileNotFound bs) =
  "Could not find the box file located at: " <> boxFileRender bs
boxErrorRender (BoxParseError e) =
  "Error parsing box file with the following error: " <> e

-- | Select a box at random so that the first box isn't the list isn't always picked again and again
selectRandomBox :: [Box] -> IO (Maybe Box)
selectRandomBox =
  fmap listToMaybe . shuffleM

gatewayFlavour :: GatewayType -> Flavour
gatewayFlavour Gateway       = Flavour "gateway"
gatewayFlavour GatewaySecure = Flavour "gatewaysecure"
