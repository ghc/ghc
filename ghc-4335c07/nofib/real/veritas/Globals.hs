module Globals where

import Tags

import Core_datatype

import Type_defs

type Global_state = ( Int , [Attribute] , Tag_table )

initial_gst :: Global_state

initial_gst = ( -1 , [] , default_tag_tbl )

set_print_depth	pd   ( _  , attL , tags ) = ( pd , attL , tags )

set_attributes	attL ( pd , _    , tags ) = ( pd , attL , tags )

set_tag_table   tags ( pd , attL , _    ) = ( pd , attL , tags )

get_print_depth	( pd , _   , _    ) = pd

get_attributes	( _ , attL , _    ) = attL

get_tag_table	( _ , _    , tags ) = tags

--different :: Global_state -> Global_state -> Bool

--different gst1 gst2 = gst1 /= gst2

fetch_ps ( _ , _ , tgL ) = tgL
