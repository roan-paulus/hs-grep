module Types (FileContents, FileBundle (..)) where

type FileContents = String
data FileBundle = FileBundle {path :: FilePath, contents :: FileContents}
