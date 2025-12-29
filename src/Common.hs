module Common where

-- A separate option datatype is used for more modular parsing.
data POptionValue = Int | Float | String | Bool 
data POption = POptionDirect ConfigOption [POptionValue] | POptionMap ConfigOption [(String, POptionValue)] 

type PLang = [PCommOpt]
data PCommOpt = PCommOpt PComm [POption]
data PComm  = PConfig String
            | PBeginEnd PBeginEndContent -- The \begin and \end tags can be detected during parsing, an removed in favour of a singular tag.
            | PTitle [PText]
            | PAuthor [PText]
            | PDate [PText]
            | PSection [PText]
            | PSubsection [PText]
            | PFigure FilePath
            | PNewpage
            | PHLine
            
-- The "document" tag is not included since there has to be only one per document.
data PBeginEndContent   = PTextblock [PText] -- A block of text is represented as a list of text data blocks, in order.
                        | PTable [[PText]]
                        | PList [PText]

data PText  = PNormal String
            | PBold String
            | PItalic String
            | PEmphasised String
            | PVerbatim String


data ConfigOption   = Size
                    | Pagenumbering
                    | Titlespacing
                    | Paragraphspacing
                    | Listspacing
                    | Tablespacing
                    | Figurespacing
                    | Spacingglue
                    | Textglue
                    | Font
                    | Parsize
                    | Titlesize
                    | Justification
                    deriving (Show, Eq)