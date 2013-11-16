import System.Environment (getArgs)
import Data.Bits          (xor)

-- Use bytestrings to deal with binary files.
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C


-- | The key to encrypt with.
key :: String
key = "GLASSCEILING"

-- | For every file in the arguments list, XOR it with the
-- | encryption/decryption key and write it out to a file.
main ::  IO [()]
main = getArgs >>= mapM encryptFile 

-- | Encrypt a file at a given filepath.
-- | Since this is just a XOR, encryption and decryption are identical.
encryptFile ::  FilePath -> IO ()
encryptFile filename = do
  -- Read the file as a binary bytestring
  contents <- B.readFile filename

  let -- Pack the key, repeating infinitely, into a bytestring
      repeatingKey = C.pack $ cycle key

      -- Encrypt using a element-wise xor
      encrypted = B.pack $ B.zipWith xor contents repeatingKey

      -- Write out to the same filename as input plus ".xor" at the end
      newName = filename ++ ".xor"

  -- Write the data to the file
  B.writeFile newName encrypted
