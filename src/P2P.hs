{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | Peer-to-peer node discovery backend for Cloud Haskell based on the TCP
-- transport. Provided with a known node address it discovers and maintains
-- the knowledge of it's peers.
--
-- > import qualified Control.Distributed.Backend.P2P as P2P
-- > import           Control.Monad.Trans (liftIO)
-- > import           Control.Concurrent (threadDelay)
-- >
-- > main = P2P.bootstrap "myhostname" "9001" [P2P.makeNodeId "seedhost:9000"] $ do
-- >     liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
-- >     P2P.nsendPeers "myService" ("some", "message")

module P2P (
    -- * Starting peer controller
    bootstrap,
    bootstrapSimple,
    bootstrapNonBlocking,
    peerController,
    -- * Nodes manipulation
    makeNodeId,
    getPeers,
    getCapable,
    nsendPeers,
    nsendCapable,
    -- * Auxiliary
    createLocalNode,
    waitController
) where

import Control.Distributed.Process                as DP
import Control.Distributed.Process.Node           as DPN
import Control.Distributed.Process.Serializable (Serializable)
import Network.Transport (EndPointAddress(..))
import Network.Socket (HostName, ServiceName)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Data.Maybe (isJust)

-- * Peer-to-peer API

type Peers = S.Set ProcessId

newtype PeerState = PeerState { p2pPeers :: MVar Peers }

initPeerState :: Process PeerState
initPeerState = do
    self <- getSelfPid
    peers <- liftIO $ newMVar (S.singleton self)
    return $! PeerState peers

-- ** Initialization

-- | Make a NodeId from "host:port" string.
makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]


-- | Start a controller service process and aquire connections to a swarm.
bootstrapSimple
  :: HostName
  -> ServiceName
  -> (ServiceName -> (HostName, ServiceName))
  -> Process ()
  -> IO ()
bootstrapSimple host servName ext prc = do
    node <- createLocalNode host servName ext initRemoteTable
    _ <- forkProcess node $ peerController []
    runProcess node $ waitController prc

    
-- | Start a controller service process and aquire connections to a swarm.
bootstrap
  :: HostName
  -> ServiceName
  -> (ServiceName -> (HostName, ServiceName))
  -> RemoteTable
  -> [NodeId]
  -> Process ()
  -> IO ()
bootstrap host servName ext rTable seeds prc = do
    node <- createLocalNode host servName ext rTable
    _ <- forkProcess node $ peerController seeds
    runProcess node $ waitController prc

-- | Like 'bootstrap' but use 'forkProcess' instead of 'runProcess'. Returns local node and pid of given process
bootstrapNonBlocking
  :: HostName
  -> ServiceName
  -> (ServiceName -> (HostName, ServiceName))
  -> RemoteTable
  -> [NodeId]
  -> Process ()
  -> IO (LocalNode, ProcessId)
bootstrapNonBlocking host port ext rTable seeds prc = do
    node <- createLocalNode host port ext rTable
    _ <- forkProcess node $ peerController seeds
    pid <- forkProcess node $ waitController prc
    return (node, pid)

-- | Waits for controller to start, then runs given process
waitController :: Process a -> Process a
waitController prc = do
    res <- whereis peerControllerService
    case res of
        Nothing -> liftIO (threadDelay 100000) >> waitController prc
        Just _ -> say "Bootstrap complete." >> prc

-- | Creates tcp local node which used by 'bootstrap'
createLocalNode
  :: HostName
  -> ServiceName
  -> (ServiceName -> (HostName, ServiceName))
  -> RemoteTable
  -> IO LocalNode
createLocalNode host port mkExternal rTable = do
    transport <- either (error . show) id
                 <$> createTransport host port mkExternal defaultTCPParameters
    newLocalNode transport rTable

peerControllerService :: String
peerControllerService = "P2P:Controller"

-- | A P2P controller service process.
peerController :: [NodeId] -> Process ()
peerController seeds = do
    state <- initPeerState

    getSelfPid >>= register peerControllerService

    mapM_ doDiscover seeds
    say "P2P controller started."
    forever $ receiveWait [ matchIf isPeerDiscover $ onDiscover state
                          , match $ onMonitor state
                          , match $ onPeerRequest state
                          , match $ onPeerQuery state
                          , match onPeerCapable
                          ]

-- ** Discovery

doDiscover :: NodeId -> Process ()
doDiscover node = do
    say $ "Examining node: " ++ show node
    whereisRemoteAsync node peerControllerService

doRegister :: PeerState -> ProcessId -> Process ()
doRegister PeerState{..} pid = do
    pids <- liftIO $ takeMVar p2pPeers
    if S.member pid pids
        then liftIO $ putMVar p2pPeers pids
        else do
            say $ "Registering peer:" ++ show pid
            _ <- monitor pid

            liftIO $ putMVar p2pPeers (S.insert pid pids)
            say $ "New node: " ++ show pid
            doDiscover $ processNodeId pid

doUnregister :: PeerState -> Maybe MonitorRef -> ProcessId -> Process ()
doUnregister PeerState{..} mref pid = do
    say $ "Unregistering peer: " ++ show pid
    maybe (return ()) unmonitor mref
    peers <- liftIO $ takeMVar p2pPeers
    liftIO $ putMVar p2pPeers (S.delete pid peers)

isPeerDiscover :: WhereIsReply -> Bool
isPeerDiscover (WhereIsReply service pid) =
    service == peerControllerService && isJust pid

onDiscover :: PeerState -> WhereIsReply -> Process ()
onDiscover _     (WhereIsReply _ Nothing) = return ()
onDiscover state (WhereIsReply _ (Just seedPid)) = do
    say $ "Peer discovered: " ++ show seedPid

    (sp, rp) <- newChan
    self <- getSelfPid
    send seedPid (self, sp :: SendPort Peers)
    say "Waiting for peers..."
    peers <- receiveChan rp

    known <- liftIO $ readMVar (p2pPeers state)
    mapM_ (doRegister state) (S.toList $ S.difference peers known)

onPeerRequest :: PeerState -> (ProcessId, SendPort Peers) -> Process ()
onPeerRequest PeerState{..} (peer, replyTo) = do
    say $ "Peer exchange with " ++ show peer
    peers <- liftIO $ takeMVar p2pPeers
    if S.member peer peers
        then liftIO $ putMVar p2pPeers peers
        else do
            _ <- monitor peer
            liftIO $ putMVar p2pPeers (S.insert peer peers)

    sendChan replyTo peers

onPeerQuery :: PeerState -> SendPort Peers -> Process ()
onPeerQuery PeerState{..} replyTo = do
    say "Local peer query."
    liftIO (readMVar p2pPeers) >>= sendChan replyTo

onPeerCapable :: (String, SendPort ProcessId) -> Process ()
onPeerCapable (service, replyTo) = do
    say $ "Capability request: " ++ service
    res <- whereis service
    case res of
        Nothing -> say "I can't."
        Just pid -> say "I can!" >> sendChan replyTo pid

onMonitor :: PeerState -> ProcessMonitorNotification -> Process ()
onMonitor state (ProcessMonitorNotification mref pid reason) = do
    say $ "Monitor event: " ++ show (pid, reason)
    doUnregister state (Just mref) pid

-- ** Discovery

-- | Get a list of currently available peer nodes.
getPeers :: Process [NodeId]
getPeers = do
    say "Requesting peer list from local controller..."
    (sp, rp) <- newChan
    nsend peerControllerService (sp :: SendPort Peers)
    map processNodeId . S.toList <$> receiveChan rp

-- | Poll a network for a list of specific service providers.
getCapable :: String -> Process [ProcessId]
getCapable service = do
    (sp, rp) <- newChan
    nsendPeers peerControllerService (service, sp)
    say "Waiting for capable nodes..."
    go rp []

    where go rp acc = do res <- receiveChanTimeout 100000 rp
                         case res of Just pid -> say "cap hit" >> go rp (pid:acc)
                                     Nothing -> say "cap done" >> return acc

-- ** Messaging

-- | Broadcast a message to a specific service on all peers.
nsendPeers :: Serializable a => String -> a -> Process ()
nsendPeers service msg = getPeers >>= mapM_ (\peer -> nsendRemote peer service msg)

-- | Broadcast a message to a service of on nodes currently running it.
nsendCapable :: Serializable a => String -> a -> Process ()
nsendCapable service msg = getCapable service >>= mapM_ (`send` msg)



