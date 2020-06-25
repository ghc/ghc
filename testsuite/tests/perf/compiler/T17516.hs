-- Reduced from Codec.MIME.String.Headers from mime-string-0.5
module T17516 (get_addr_spec, get_to) where

import Prelude hiding ( (<*>), (<$>), (<*), (<$) )
import Data.Char
import Data.List (intersperse)

import T17516A

-----------------------
-- Utils

ignore :: Parser inp a -> Parser inp ()
ignore p = () <$  p

boxp :: Parser inp a -> Parser inp [a]
boxp p = box <$> p

-----------------------
-- RFC 2234

p_CTL :: Parser Char Char
p_CTL = pPred (\c -> ord c < 32 || ord c == 127)

p_SP :: Parser Char Char
p_SP = pChar ' '

p_HTAB :: Parser Char Char
p_HTAB = pChar '\t'

p_WSP :: Parser Char Char
p_WSP = p_SP <|> p_HTAB

-----------------------
-- RFC 2822

p_NO_WS_CTL :: Parser Char Char
p_NO_WS_CTL = pPred (\c -> let o = ord c in 1 <= o && o <= 8
                                         || o == 11
                                         || o == 12
                                         || 14 <= o && o <= 31
                                         || o == 127)

-- If we follow the spec precisely then we get pMany (pMany), and hence
-- non-termination, so we merge the definition of p_obs_text in.
p_text :: Parser Char String
p_text = concat
     <$> pMany (
                    p_encoded_words
                <|  boxp (pPred (\c -> let o = ord c in 0 <= o && o <= 9
                                                     || o == 11
                                                     || o == 12
                                                     || 14 <= o && o <= 127))
               )

-- We are lax about checking they have any necessary surrounding
-- whitespace
p_encoded_words :: Parser Char String
p_encoded_words = (\x xs -> x ++ concat xs)
              <$> p_encoded_word
              <*> pMany (id <$  cws <*> p_encoded_word)

-- XXX What happens if iconv doesn't understand the charset "cs"?
p_encoded_word :: Parser Char String
p_encoded_word = (\_ dec text -> dec text)
             <$  pString "=?"
             <*> p_charset
             <*  pChar '?'
             <*> p_encoding
             <*  pChar '?'
             <*> p_encoded_text
             <*  pString "?="

-- token definition inlined as they use a different one to p_token.
p_charset :: Parser Char String
p_charset = pAtLeast 1 (pPred isAscii <!> (p_SP <|> p_CTL <|> p_especials))

p_especials :: Parser Char Char
p_especials = pPred (`elem` "()<>@,;:\\\"/[]?.=")

-- This is much stricter than specified, but if it's not [qQbB] then
-- we'd want to fall back to showing it as a string anyway.
p_encoding :: Parser Char (String -> String)
p_encoding = id <$  (pChar 'Q' <|> pChar 'q')
         <|> id <$  (pChar 'B' <|> pChar 'b')

p_encoded_text :: Parser Char String
p_encoded_text = pMany (pPred (\c -> isAsciiPrint c && c /= '?' && c /= ' '))

p_quoted_pair :: Parser Char String
p_quoted_pair = id <$  pChar '\\' <*> p_text <|> boxp p_obs_qp

p_obs_qp :: Parser Char Char
p_obs_qp = id <$  pChar '\\' <*> pPred isAscii

-- Done differently as the newlines are already gone
p_FWS :: Parser Char String
p_FWS = pMany p_WSP

p_ctext :: Parser Char Char
p_ctext = p_NO_WS_CTL
      <|> pPred (\c -> let o = ord c in 33 <= o && o <= 39
                                     || 42 <= o && o <= 91
                                     || 93 <= o && o <= 126)

p_ccontent :: Parser Char ()
p_ccontent = ignore p_ctext <|> ignore p_quoted_pair <|> p_comment

p_comment :: Parser Char ()
p_comment = ()
        <$  pChar '('
        <*  pMany (() <$  pMany p_NO_WS_CTL <*  p_ccontent)
        <*  pMany p_NO_WS_CTL
        <*  pChar ')'

-- We might want to keep the result. If we do then we also need to
-- handle encoded words properly.
-- This isn't quite CFWS as we need to be able to accept "1.0"
-- as a MIME version with cws between all the characters.
-- Also, we've already removed all the newlines in the headers.
cws :: Parser Char ()
cws = ignore $ pMany (ignore (pAtLeast 1 p_WSP) <|> p_comment)

p_qtext :: Parser Char Char
p_qtext = p_NO_WS_CTL
      <|> pPred (\c -> let o = ord c in o == 33
                                     || 35 <= o && o <= 91
                                     || 93 <= o && o <= 126)

p_qcontent :: Parser Char String
p_qcontent = boxp p_qtext
         <|> p_quoted_pair

p_quoted_string :: Parser Char String
p_quoted_string = (++)
              <$  cws
              <*  pChar '"'
              <*> (concat <$> pMany ((++) <$> pOptDef "" p_FWS <*> p_qcontent))
              <*> pOptDef "" p_FWS
              <*  pChar '"'

p_dcontent :: Parser Char String
p_dcontent = boxp p_dtext <|> p_quoted_pair

p_dtext :: Parser Char Char
p_dtext = p_NO_WS_CTL
      <|> pPred (\c -> let o = ord c in 33 <= o && o <= 90
                                     || 94 <= o && o <= 126)

p_atom :: Parser Char String
p_atom = id
     <$  cws
     <*> pAtLeast 1 p_atext
     <*  cws

p_atext :: Parser Char Char
p_atext = pPred (\c -> isAsciiAlphaNum c || c `elem` "!#$%&'+-/=?^_`{|}~")

p_dot_atom :: Parser Char String
p_dot_atom = id
         <$  cws
         <*> p_dot_atom_text
         <*  cws

p_word :: Parser Char String
p_word = p_atom <|> p_quoted_string

-- This incorporates obs-phrase
p_phrase :: Parser Char [String]
p_phrase = (:)
       <$> (p_encoded_words <|  p_word)
       <*> pMany (id <$  cws <*> (p_encoded_words <|  p_word <|  pString "."))
   <|> boxp p_quoted_string

p_dot_atom_text :: Parser Char String
p_dot_atom_text = (\x xs -> x ++ concat xs)
              <$> pAtLeast 1 p_atext
              <*> pMany ((:) <$> pChar '.' <*> pAtLeast 1 p_atext)

p_local_part :: Parser Char String
p_local_part = p_dot_atom <|> p_quoted_string <|> p_obs_local_part

p_obs_local_part :: Parser Char String
p_obs_local_part = (\x xs -> x ++ concat xs)
               <$> p_word
               <*> pMany ((:) <$> pChar '.' <*> p_word)

p_domain :: Parser Char Domain
p_domain = Domain <$> p_dot_atom <|> p_domain_literal <|> p_obs_domain

p_domain_literal :: Parser Char Domain
p_domain_literal = (LiteralDomain . concat)
               <$  cws
               <*  pChar '['
               <*> pMany (    id
                          <$  p_FWS
                          <*> p_dcontent)
               <*  p_FWS
               <*  pChar ']'
               <*  cws

p_obs_domain :: Parser Char Domain
p_obs_domain = (\x xs -> Domain (x ++ concat xs))
           <$> p_atom
           <*> pMany ((:) <$> pChar '.' <*> p_atom)

data Domain = Domain String | LiteralDomain String
    deriving (Show, Read, Eq)

newtype To = To [Address]
    deriving (Show, Read)

data Address = Address Mailbox
             | Group String [Mailbox]
    deriving (Show, Read)

get_to :: String -> Maybe To
get_to xs
 = case parse ph_to xs of
       Left t -> Just t
       Right _ -> Nothing

ph_to :: Parser Char To
ph_to = To <$  cws <*> p_address_list <*  cws <*  pEOI

-- obs-addr-list merged in
p_address_list :: Parser Char [Address]
p_address_list = (:)
             <$  pMany (() <$  pChar ',' <*  cws)
             <*> p_address
             <*> pMany (    id
                        <$  pAtLeast 1 (() <$  cws <*  pChar ',')
                        <*  cws
                        <*> p_address)
             <*  pMany (() <$  cws <*  pChar ',')

p_address :: Parser Char Address
p_address = Address <$> p_mailbox
        <|> p_group

p_group :: Parser Char Address
p_group = Group
      <$> p_display_name
      <*  cws
      <*  pChar ':'
      <*  cws
      <*> pOptDef [] p_mailbox_list
      <*  cws
      <*  pChar ';'

-- obs-mbox-list merged in
p_mailbox_list :: Parser Char [Mailbox]
p_mailbox_list = (:)
             <$  pMany (() <$  pChar ',' <*  cws)
             <*> p_mailbox
             <*> pMany (    id
                        <$  pAtLeast 1 (() <$  cws <*  pChar ',')
                        <*  cws
                        <*> p_mailbox)
             <*  pMany (() <$  cws <*  pChar ',')

data Mailbox = Mailbox (Maybe String) RoutedEmailAddress
    deriving (Show, Read, Eq)

p_mailbox :: Parser Char Mailbox
p_mailbox = p_name_addr
        <|> (Mailbox Nothing . NormalEmailAddress) <$> p_addr_spec

p_name_addr :: Parser Char Mailbox
p_name_addr = Mailbox
          <$> pMaybe p_display_name
          <*  cws
          <*> p_angle_addr

data EmailAddress = EmailAddress String Domain
    deriving (Show, Read, Eq)

data RoutedEmailAddress = NormalEmailAddress          EmailAddress
                        | RoutedEmailAddress [Domain] EmailAddress
    deriving (Show, Read, Eq)

p_angle_addr :: Parser Char RoutedEmailAddress
p_angle_addr = ($)
           <$  pChar '<'
           <*  cws
           -- This next makes us also satisfy obs-angle-addr
           <*> pOptDef NormalEmailAddress
                       (RoutedEmailAddress <$> p_obs_route <*  cws)
           <*> p_addr_spec
           <*  cws
           <*  pChar '>'

get_addr_spec :: String -> Maybe EmailAddress
get_addr_spec xs
 = case parse p_addr_spec xs of
       Left e -> Just e
       Right _ -> Nothing

p_addr_spec :: Parser Char EmailAddress
p_addr_spec  = EmailAddress
           <$> p_local_part
           <*  cws
           <*  pChar '@'
           <*  cws
           <*> p_domain

p_display_name :: Parser Char String
p_display_name = (concat . intersperse " ") <$> p_phrase

p_obs_route :: Parser Char [Domain]
p_obs_route = id <$> p_obs_domain_list <*  pChar ':'

p_obs_domain_list :: Parser Char [Domain]
p_obs_domain_list = (:)
                <$  pChar '@'
                <*  cws
                <*> p_domain
                <*> pMany (    id
                           <$  pMaybe (() <$  cws <*  pChar ',')
                           <*  cws
                           <*  pChar '@'
                           <*  cws
                           <*> p_domain)

-- Utils

isAsciiPrint :: Char -> Bool
isAsciiPrint c = isAscii c && isPrint c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

box :: a -> [a]
box x = [x]
