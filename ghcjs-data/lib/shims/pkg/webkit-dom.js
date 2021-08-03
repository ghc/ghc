// Graphics.UI.Gtk.WebKit.DOM.Xpath
h$webkit_dom_xpath_result_get_type = (function()
                                      {
                                        return h$g_get_type(XPathResult);
                                      });
var h$webkit_dom_xpath_result_iterate_next;
h$webkit_dom_xpath_result_iterate_next = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["iterateNext"]();
                                          });
var h$webkit_dom_xpath_result_snapshot_item;
h$webkit_dom_xpath_result_snapshot_item = (function(self,
                                           self_2, index)
                                           {
                                             h$ret1 = 0;
                                             return self["snapshotItem"](index);
                                           });
var h$webkit_dom_xpath_result_get_result_type;
h$webkit_dom_xpath_result_get_result_type = (function(self,
                                             self_2)
                                             {
                                               return self["resultType"];
                                             });
var h$webkit_dom_xpath_result_get_number_value;
h$webkit_dom_xpath_result_get_number_value = (function(self,
                                              self_2)
                                              {
                                                return self["numberValue"];
                                              });
var h$webkit_dom_xpath_result_get_string_value;
h$webkit_dom_xpath_result_get_string_value = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["stringValue"]);
                                              });
var h$webkit_dom_xpath_result_get_boolean_value;
h$webkit_dom_xpath_result_get_boolean_value = (function(self,
                                               self_2)
                                               {
                                                 return self["booleanValue"];
                                               });
var h$webkit_dom_xpath_result_get_single_node_value;
h$webkit_dom_xpath_result_get_single_node_value = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["singleNodeValue"];
                                                   });
var h$webkit_dom_xpath_result_get_invalid_iterator_state;
h$webkit_dom_xpath_result_get_invalid_iterator_state = (function(self,
                                                        self_2)
                                                        {
                                                          return self["invalidIteratorState"];
                                                        });
var h$webkit_dom_xpath_result_get_snapshot_length;
h$webkit_dom_xpath_result_get_snapshot_length = (function(self,
                                                 self_2)
                                                 {
                                                   return self["snapshotLength"];
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Xpath
h$webkit_dom_xpath_ns_resolver_get_type = (function()
                                           {
                                             return h$g_get_type(XPathNSResolver);
                                           });
var h$webkit_dom_xpath_ns_resolver_lookup_namespace_uri;
h$webkit_dom_xpath_ns_resolver_lookup_namespace_uri = (function(self,
                                                       self_2, prefix, prefix_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return h$encodeUtf8(self["lookupNamespaceURI"](h$decodeUtf8z(prefix,
                                                         prefix_2)));
                                                       });
// Graphics.UI.Gtk.WebKit.DOM.Xpath
h$webkit_dom_xpath_expression_get_type = (function()
                                          {
                                            return h$g_get_type(XPathExpression);
                                          });
var h$webkit_dom_xpath_expression_evaluate;
h$webkit_dom_xpath_expression_evaluate = (function(self,
                                          self_2, contextNode,
                                          contextNode_2, type, inResult,
                                          inResult_2)
                                          {
                                            h$ret1 = 0;
                                            return self["evaluate"](contextNode,
                                            type, inResult);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Xml
h$webkit_dom_xml_http_request_get_type = (function()
                                          {
                                            return h$g_get_type(XMLHttpRequest);
                                          });
var h$webkit_dom_xml_http_request_set_request_header;
h$webkit_dom_xml_http_request_set_request_header = (function(self,
                                                    self_2, header, header_2,
                                                    value, value_2)
                                                    {
                                                      return self["setRequestHeader"](h$decodeUtf8z(header,
                                                      header_2),
                                                      h$decodeUtf8z(value,
                                                      value_2));
                                                    });
var h$webkit_dom_xml_http_request_abort;
h$webkit_dom_xml_http_request_abort = (function(self,
                                       self_2)
                                       {
                                         return self["abort"]();
                                       });
var h$webkit_dom_xml_http_request_get_all_response_headers;
h$webkit_dom_xml_http_request_get_all_response_headers = (function(self,
                                                          self_2)
                                                          {
                                                            h$ret1 = 0;
                                                            return h$encodeUtf8(self["getAllResponseHeaders"]());
                                                          });
var h$webkit_dom_xml_http_request_get_response_header;
h$webkit_dom_xml_http_request_get_response_header = (function(self,
                                                     self_2, header, header_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["getResponseHeader"](h$decodeUtf8z(header,
                                                       header_2)));
                                                     });
var h$webkit_dom_xml_http_request_override_mime_type;
h$webkit_dom_xml_http_request_override_mime_type = (function(self,
                                                    self_2, override,
                                                    override_2)
                                                    {
                                                      return self["overrideMimeType"](h$decodeUtf8z(override,
                                                      override_2));
                                                    });
var h$webkit_dom_xml_http_request_dispatch_event;
h$webkit_dom_xml_http_request_dispatch_event = (function(self,
                                                self_2, evt, evt_2)
                                                {
                                                  return self["dispatchEvent"](evt);
                                                });
var h$webkit_dom_xml_http_request_set_onabort;
h$webkit_dom_xml_http_request_set_onabort = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["onabort"] = val;
                                             });
var h$webkit_dom_xml_http_request_get_onabort;
h$webkit_dom_xml_http_request_get_onabort = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["onabort"];
                                             });
var h$webkit_dom_xml_http_request_set_onerror;
h$webkit_dom_xml_http_request_set_onerror = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["onerror"] = val;
                                             });
var h$webkit_dom_xml_http_request_get_onerror;
h$webkit_dom_xml_http_request_get_onerror = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["onerror"];
                                             });
var h$webkit_dom_xml_http_request_set_onload;
h$webkit_dom_xml_http_request_set_onload = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["onload"] = val;
                                            });
var h$webkit_dom_xml_http_request_get_onload;
h$webkit_dom_xml_http_request_get_onload = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["onload"];
                                            });
var h$webkit_dom_xml_http_request_set_onloadend;
h$webkit_dom_xml_http_request_set_onloadend = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["onloadend"] = val;
                                               });
var h$webkit_dom_xml_http_request_get_onloadend;
h$webkit_dom_xml_http_request_get_onloadend = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["onloadend"];
                                               });
var h$webkit_dom_xml_http_request_set_onloadstart;
h$webkit_dom_xml_http_request_set_onloadstart = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["onloadstart"] = val;
                                                 });
var h$webkit_dom_xml_http_request_get_onloadstart;
h$webkit_dom_xml_http_request_get_onloadstart = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["onloadstart"];
                                                 });
var h$webkit_dom_xml_http_request_set_onprogress;
h$webkit_dom_xml_http_request_set_onprogress = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["onprogress"] = val;
                                                });
var h$webkit_dom_xml_http_request_get_onprogress;
h$webkit_dom_xml_http_request_get_onprogress = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["onprogress"];
                                                });
var h$webkit_dom_xml_http_request_set_onreadystatechange;
h$webkit_dom_xml_http_request_set_onreadystatechange = (function(self,
                                                        self_2, val, val_2)
                                                        {
                                                          self["onreadystatechange"] = val;
                                                        });
var h$webkit_dom_xml_http_request_get_onreadystatechange;
h$webkit_dom_xml_http_request_get_onreadystatechange = (function(self,
                                                        self_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["onreadystatechange"];
                                                        });
var h$webkit_dom_xml_http_request_get_ready_state;
h$webkit_dom_xml_http_request_get_ready_state = (function(self,
                                                 self_2)
                                                 {
                                                   return self["readyState"];
                                                 });
var h$webkit_dom_xml_http_request_set_with_credentials;
h$webkit_dom_xml_http_request_set_with_credentials = (function(self,
                                                      self_2, val)
                                                      {
                                                        self["withCredentials"] = val;
                                                      });
var h$webkit_dom_xml_http_request_get_with_credentials;
h$webkit_dom_xml_http_request_get_with_credentials = (function(self,
                                                      self_2)
                                                      {
                                                        return self["withCredentials"];
                                                      });
var h$webkit_dom_xml_http_request_get_upload;
h$webkit_dom_xml_http_request_get_upload = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["upload"];
                                            });
var h$webkit_dom_xml_http_request_get_response_xml;
h$webkit_dom_xml_http_request_get_response_xml = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["responseXML"];
                                                  });
var h$webkit_dom_xml_http_request_set_response_type;
h$webkit_dom_xml_http_request_set_response_type = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["responseType"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_xml_http_request_get_response_type;
h$webkit_dom_xml_http_request_get_response_type = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["responseType"]);
                                                   });
var h$webkit_dom_xml_http_request_get_status;
h$webkit_dom_xml_http_request_get_status = (function(self,
                                            self_2)
                                            {
                                              return self["status"];
                                            });
var h$webkit_dom_xml_http_request_get_status_text;
h$webkit_dom_xml_http_request_get_status_text = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["statusText"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_webkit_point_get_type = (function()
                                      {
                                        return h$g_get_type(WebKitPoint);
                                      });
var h$webkit_dom_webkit_point_set_x;
h$webkit_dom_webkit_point_set_x = (function(self,
                                   self_2, val)
                                   {
                                     self["x"] = val;
                                   });
var h$webkit_dom_webkit_point_get_x;
h$webkit_dom_webkit_point_get_x = (function(self,
                                   self_2)
                                   {
                                     return self["x"];
                                   });
var h$webkit_dom_webkit_point_set_y;
h$webkit_dom_webkit_point_set_y = (function(self,
                                   self_2, val)
                                   {
                                     self["y"] = val;
                                   });
var h$webkit_dom_webkit_point_get_y;
h$webkit_dom_webkit_point_get_y = (function(self,
                                   self_2)
                                   {
                                     return self["y"];
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_webkit_named_flow_get_type = (function()
                                           {
                                             return h$g_get_type(WebKitNamedFlow);
                                           });
var h$webkit_dom_webkit_named_flow_get_regions_by_content;
h$webkit_dom_webkit_named_flow_get_regions_by_content = (function(self,
                                                         self_2, contentNode,
                                                         contentNode_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return self["getRegionsByContent"](contentNode);
                                                         });
var h$webkit_dom_webkit_named_flow_get_regions;
h$webkit_dom_webkit_named_flow_get_regions = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["getRegions"]();
                                              });
var h$webkit_dom_webkit_named_flow_get_content;
h$webkit_dom_webkit_named_flow_get_content = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["getContent"]();
                                              });
var h$webkit_dom_webkit_named_flow_dispatch_event;
h$webkit_dom_webkit_named_flow_dispatch_event = (function(self,
                                                 self_2, event, event_2)
                                                 {
                                                   return self["dispatchEvent"](event);
                                                 });
var h$webkit_dom_webkit_named_flow_get_name;
h$webkit_dom_webkit_named_flow_get_name = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["name"]);
                                           });
var h$webkit_dom_webkit_named_flow_get_overset;
h$webkit_dom_webkit_named_flow_get_overset = (function(self,
                                              self_2)
                                              {
                                                return self["overset"];
                                              });
var h$webkit_dom_webkit_named_flow_get_first_empty_region_index;
h$webkit_dom_webkit_named_flow_get_first_empty_region_index = (function(self,
                                                               self_2)
                                                               {
                                                                 return self["firstEmptyRegionIndex"];
                                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_webkit_animation_list_get_type = (function()
                                               {
                                                 return h$g_get_type(WebKitAnimationList);
                                               });
var h$webkit_dom_webkit_animation_list_item;
h$webkit_dom_webkit_animation_list_item = (function(self,
                                           self_2, index)
                                           {
                                             h$ret1 = 0;
                                             return self["item"](index);
                                           });
var h$webkit_dom_webkit_animation_list_get_length;
h$webkit_dom_webkit_animation_list_get_length = (function(self,
                                                 self_2)
                                                 {
                                                   return self["length"];
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_webkit_animation_get_type = (function()
                                          {
                                            return h$g_get_type(WebKitAnimation);
                                          });
var h$webkit_dom_webkit_animation_play;
h$webkit_dom_webkit_animation_play = (function(self,
                                      self_2)
                                      {
                                        return self["play"]();
                                      });
var h$webkit_dom_webkit_animation_pause;
h$webkit_dom_webkit_animation_pause = (function(self,
                                       self_2)
                                       {
                                         return self["pause"]();
                                       });
var h$webkit_dom_webkit_animation_get_name;
h$webkit_dom_webkit_animation_get_name = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["name"]);
                                          });
var h$webkit_dom_webkit_animation_get_duration;
h$webkit_dom_webkit_animation_get_duration = (function(self,
                                              self_2)
                                              {
                                                return self["duration"];
                                              });
var h$webkit_dom_webkit_animation_set_elapsed_time;
h$webkit_dom_webkit_animation_set_elapsed_time = (function(self,
                                                  self_2, val)
                                                  {
                                                    self["elapsedTime"] = val;
                                                  });
var h$webkit_dom_webkit_animation_get_elapsed_time;
h$webkit_dom_webkit_animation_get_elapsed_time = (function(self,
                                                  self_2)
                                                  {
                                                    return self["elapsedTime"];
                                                  });
var h$webkit_dom_webkit_animation_get_delay;
h$webkit_dom_webkit_animation_get_delay = (function(self,
                                           self_2)
                                           {
                                             return self["delay"];
                                           });
var h$webkit_dom_webkit_animation_get_paused;
h$webkit_dom_webkit_animation_get_paused = (function(self,
                                            self_2)
                                            {
                                              return self["paused"];
                                            });
var h$webkit_dom_webkit_animation_get_ended;
h$webkit_dom_webkit_animation_get_ended = (function(self,
                                           self_2)
                                           {
                                             return self["ended"];
                                           });
var h$webkit_dom_webkit_animation_get_direction;
h$webkit_dom_webkit_animation_get_direction = (function(self,
                                               self_2)
                                               {
                                                 return self["direction"];
                                               });
var h$webkit_dom_webkit_animation_get_fill_mode;
h$webkit_dom_webkit_animation_get_fill_mode = (function(self,
                                               self_2)
                                               {
                                                 return self["fillMode"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_ui_event_get_type = (function()
                                  {
                                    return h$g_get_type(UIEvent);
                                  });
var h$webkit_dom_ui_event_init_ui_event;
h$webkit_dom_ui_event_init_ui_event = (function(self,
                                       self_2, type, type_2, canBubble,
                                       cancelable, view, view_2,
                                       detail)
                                       {
                                         return self["initUIEvent"](h$decodeUtf8z(type,
                                         type_2), canBubble, cancelable,
                                         view, detail);
                                       });
var h$webkit_dom_ui_event_get_view;
h$webkit_dom_ui_event_get_view = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return self["view"];
                                  });
var h$webkit_dom_ui_event_get_detail;
h$webkit_dom_ui_event_get_detail = (function(self,
                                    self_2)
                                    {
                                      return self["detail"];
                                    });
var h$webkit_dom_ui_event_get_key_code;
h$webkit_dom_ui_event_get_key_code = (function(self,
                                      self_2)
                                      {
                                        return self["keyCode"];
                                      });
var h$webkit_dom_ui_event_get_char_code;
h$webkit_dom_ui_event_get_char_code = (function(self,
                                       self_2)
                                       {
                                         return self["charCode"];
                                       });
var h$webkit_dom_ui_event_get_layer_x;
h$webkit_dom_ui_event_get_layer_x = (function(self,
                                     self_2)
                                     {
                                       return self["layerX"];
                                     });
var h$webkit_dom_ui_event_get_layer_y;
h$webkit_dom_ui_event_get_layer_y = (function(self,
                                     self_2)
                                     {
                                       return self["layerY"];
                                     });
var h$webkit_dom_ui_event_get_page_x;
h$webkit_dom_ui_event_get_page_x = (function(self,
                                    self_2)
                                    {
                                      return self["pageX"];
                                    });
var h$webkit_dom_ui_event_get_page_y;
h$webkit_dom_ui_event_get_page_y = (function(self,
                                    self_2)
                                    {
                                      return self["pageY"];
                                    });
var h$webkit_dom_ui_event_get_which;
h$webkit_dom_ui_event_get_which = (function(self,
                                   self_2)
                                   {
                                     return self["which"];
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_validity_state_get_type = (function()
                                        {
                                          return h$g_get_type(ValidityState);
                                        });
var h$webkit_dom_validity_state_get_value_missing;
h$webkit_dom_validity_state_get_value_missing = (function(self,
                                                 self_2)
                                                 {
                                                   return self["valueMissing"];
                                                 });
var h$webkit_dom_validity_state_get_type_mismatch;
h$webkit_dom_validity_state_get_type_mismatch = (function(self,
                                                 self_2)
                                                 {
                                                   return self["typeMismatch"];
                                                 });
var h$webkit_dom_validity_state_get_pattern_mismatch;
h$webkit_dom_validity_state_get_pattern_mismatch = (function(self,
                                                    self_2)
                                                    {
                                                      return self["patternMismatch"];
                                                    });
var h$webkit_dom_validity_state_get_too_long;
h$webkit_dom_validity_state_get_too_long = (function(self,
                                            self_2)
                                            {
                                              return self["tooLong"];
                                            });
var h$webkit_dom_validity_state_get_range_underflow;
h$webkit_dom_validity_state_get_range_underflow = (function(self,
                                                   self_2)
                                                   {
                                                     return self["rangeUnderflow"];
                                                   });
var h$webkit_dom_validity_state_get_range_overflow;
h$webkit_dom_validity_state_get_range_overflow = (function(self,
                                                  self_2)
                                                  {
                                                    return self["rangeOverflow"];
                                                  });
var h$webkit_dom_validity_state_get_step_mismatch;
h$webkit_dom_validity_state_get_step_mismatch = (function(self,
                                                 self_2)
                                                 {
                                                   return self["stepMismatch"];
                                                 });
var h$webkit_dom_validity_state_get_custom_error;
h$webkit_dom_validity_state_get_custom_error = (function(self,
                                                self_2)
                                                {
                                                  return self["customError"];
                                                });
var h$webkit_dom_validity_state_get_valid;
h$webkit_dom_validity_state_get_valid = (function(self,
                                         self_2)
                                         {
                                           return self["valid"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Traversal
h$webkit_dom_tree_walker_get_type = (function()
                                     {
                                       return h$g_get_type(TreeWalker);
                                     });
var h$webkit_dom_tree_walker_get_root;
h$webkit_dom_tree_walker_get_root = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["root"];
                                     });
var h$webkit_dom_tree_walker_get_what_to_show;
h$webkit_dom_tree_walker_get_what_to_show = (function(self,
                                             self_2)
                                             {
                                               return self["whatToShow"];
                                             });
var h$webkit_dom_tree_walker_get_filter;
h$webkit_dom_tree_walker_get_filter = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["filter"];
                                       });
var h$webkit_dom_tree_walker_get_expand_entity_references;
h$webkit_dom_tree_walker_get_expand_entity_references = (function(self,
                                                         self_2)
                                                         {
                                                           return self["expandEntityReferences"];
                                                         });
var h$webkit_dom_tree_walker_set_current_node;
h$webkit_dom_tree_walker_set_current_node = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["currentNode"] = val;
                                             });
var h$webkit_dom_tree_walker_get_current_node;
h$webkit_dom_tree_walker_get_current_node = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["currentNode"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_time_ranges_get_type = (function()
                                     {
                                       return h$g_get_type(TimeRanges);
                                     });
var h$webkit_dom_time_ranges_start;
h$webkit_dom_time_ranges_start = (function(self,
                                  self_2, index)
                                  {
                                    return self["start"](index);
                                  });
var h$webkit_dom_time_ranges_end;
h$webkit_dom_time_ranges_end = (function(self,
                                self_2, index)
                                {
                                  return self["end"](index);
                                });
var h$webkit_dom_time_ranges_get_length;
h$webkit_dom_time_ranges_get_length = (function(self,
                                       self_2)
                                       {
                                         return self["length"];
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_text_get_type = (function()
                              {
                                return h$g_get_type(Text);
                              });
var h$webkit_dom_text_split_text;
h$webkit_dom_text_split_text = (function(self,
                                self_2, offset)
                                {
                                  h$ret1 = 0;
                                  return self["splitText"](offset);
                                });
var h$webkit_dom_text_replace_whole_text;
h$webkit_dom_text_replace_whole_text = (function(self,
                                        self_2, content, content_2)
                                        {
                                          h$ret1 = 0;
                                          return self["replaceWholeText"](h$decodeUtf8z(content,
                                          content_2));
                                        });
var h$webkit_dom_text_get_whole_text;
h$webkit_dom_text_get_whole_text = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["wholeText"]);
                                    });
// Graphics.UI.Gtk.WebKit.DOM.Storage
h$webkit_dom_storage_info_get_type = (function()
                                      {
                                        return h$g_get_type(StorageInfo);
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Storage
h$webkit_dom_storage_get_type = (function()
                                 {
                                   return h$g_get_type(Storage);
                                 });
var h$webkit_dom_storage_key;
h$webkit_dom_storage_key = (function(self,
                            self_2, index)
                            {
                              h$ret1 = 0;
                              return h$encodeUtf8(self["key"](index));
                            });
var h$webkit_dom_storage_get_item;
h$webkit_dom_storage_get_item = (function(self,
                                 self_2, key, key_2)
                                 {
                                   h$ret1 = 0;
                                   return h$encodeUtf8(self["getItem"](h$decodeUtf8z(key,
                                   key_2)));
                                 });
var h$webkit_dom_storage_set_item;
h$webkit_dom_storage_set_item = (function(self,
                                 self_2, key, key_2, data,
                                 data_2)
                                 {
                                   return self["setItem"](h$decodeUtf8z(key,
                                   key_2), h$decodeUtf8z(data,
                                   data_2));
                                 });
var h$webkit_dom_storage_remove_item;
h$webkit_dom_storage_remove_item = (function(self,
                                    self_2, key, key_2)
                                    {
                                      return self["removeItem"](h$decodeUtf8z(key,
                                      key_2));
                                    });
var h$webkit_dom_storage_clear;
h$webkit_dom_storage_clear = (function(self,
                              self_2)
                              {
                                return self["clear"]();
                              });
var h$webkit_dom_storage_get_length;
h$webkit_dom_storage_get_length = (function(self,
                                   self_2)
                                   {
                                     return self["length"];
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Stylesheets
h$webkit_dom_style_sheet_list_get_type = (function()
                                          {
                                            return h$g_get_type(StyleSheetList);
                                          });
var h$webkit_dom_style_sheet_list_item;
h$webkit_dom_style_sheet_list_item = (function(self,
                                      self_2, index)
                                      {
                                        h$ret1 = 0;
                                        return self["item"](index);
                                      });
var h$webkit_dom_style_sheet_list_get_length;
h$webkit_dom_style_sheet_list_get_length = (function(self,
                                            self_2)
                                            {
                                              return self["length"];
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Stylesheets
h$webkit_dom_style_sheet_get_type = (function()
                                     {
                                       return h$g_get_type(StyleSheet);
                                     });
var h$webkit_dom_style_sheet_set_disabled;
h$webkit_dom_style_sheet_set_disabled = (function(self,
                                         self_2, val)
                                         {
                                           self["disabled"] = val;
                                         });
var h$webkit_dom_style_sheet_get_disabled;
h$webkit_dom_style_sheet_get_disabled = (function(self,
                                         self_2)
                                         {
                                           return self["disabled"];
                                         });
var h$webkit_dom_style_sheet_get_owner_node;
h$webkit_dom_style_sheet_get_owner_node = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["ownerNode"];
                                           });
var h$webkit_dom_style_sheet_get_parent_style_sheet;
h$webkit_dom_style_sheet_get_parent_style_sheet = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["parentStyleSheet"];
                                                   });
var h$webkit_dom_style_sheet_get_href;
h$webkit_dom_style_sheet_get_href = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return h$encodeUtf8(self["href"]);
                                     });
var h$webkit_dom_style_sheet_get_title;
h$webkit_dom_style_sheet_get_title = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["title"]);
                                      });
var h$webkit_dom_style_sheet_get_media;
h$webkit_dom_style_sheet_get_media = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["media"];
                                      });
// Graphics.UI.Gtk.WebKit.DOM.View
h$webkit_dom_style_media_get_type = (function()
                                     {
                                       return h$g_get_type(StyleMedia);
                                     });
var h$webkit_dom_style_media_match_medium;
h$webkit_dom_style_media_match_medium = (function(self,
                                         self_2, mediaquery,
                                         mediaquery_2)
                                         {
                                           return self["matchMedium"](h$decodeUtf8z(mediaquery,
                                           mediaquery_2));
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_screen_get_type = (function()
                                {
                                  return h$g_get_type(Screen);
                                });
var h$webkit_dom_screen_get_height;
h$webkit_dom_screen_get_height = (function(self,
                                  self_2)
                                  {
                                    return self["height"];
                                  });
var h$webkit_dom_screen_get_width;
h$webkit_dom_screen_get_width = (function(self,
                                 self_2)
                                 {
                                   return self["width"];
                                 });
var h$webkit_dom_screen_get_color_depth;
h$webkit_dom_screen_get_color_depth = (function(self,
                                       self_2)
                                       {
                                         return self["colorDepth"];
                                       });
var h$webkit_dom_screen_get_pixel_depth;
h$webkit_dom_screen_get_pixel_depth = (function(self,
                                       self_2)
                                       {
                                         return self["pixelDepth"];
                                       });
var h$webkit_dom_screen_get_avail_left;
h$webkit_dom_screen_get_avail_left = (function(self,
                                      self_2)
                                      {
                                        return self["availLeft"];
                                      });
var h$webkit_dom_screen_get_avail_top;
h$webkit_dom_screen_get_avail_top = (function(self,
                                     self_2)
                                     {
                                       return self["availTop"];
                                     });
var h$webkit_dom_screen_get_avail_height;
h$webkit_dom_screen_get_avail_height = (function(self,
                                        self_2)
                                        {
                                          return self["availHeight"];
                                        });
var h$webkit_dom_screen_get_avail_width;
h$webkit_dom_screen_get_avail_width = (function(self,
                                       self_2)
                                       {
                                         return self["availWidth"];
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Ranges
h$webkit_dom_range_get_type = (function()
                               {
                                 return h$g_get_type(Range);
                               });
var h$webkit_dom_range_set_start;
h$webkit_dom_range_set_start = (function(self,
                                self_2, refNode, refNode_2,
                                offset)
                                {
                                  return self["setStart"](refNode,
                                  offset);
                                });
var h$webkit_dom_range_set_end;
h$webkit_dom_range_set_end = (function(self,
                              self_2, refNode, refNode_2,
                              offset)
                              {
                                return self["setEnd"](refNode,
                                offset);
                              });
var h$webkit_dom_range_set_start_before;
h$webkit_dom_range_set_start_before = (function(self,
                                       self_2, refNode, refNode_2)
                                       {
                                         return self["setStartBefore"](refNode);
                                       });
var h$webkit_dom_range_set_start_after;
h$webkit_dom_range_set_start_after = (function(self,
                                      self_2, refNode, refNode_2)
                                      {
                                        return self["setStartAfter"](refNode);
                                      });
var h$webkit_dom_range_set_end_before;
h$webkit_dom_range_set_end_before = (function(self,
                                     self_2, refNode, refNode_2)
                                     {
                                       return self["setEndBefore"](refNode);
                                     });
var h$webkit_dom_range_set_end_after;
h$webkit_dom_range_set_end_after = (function(self,
                                    self_2, refNode, refNode_2)
                                    {
                                      return self["setEndAfter"](refNode);
                                    });
var h$webkit_dom_range_collapse;
h$webkit_dom_range_collapse = (function(self,
                               self_2, toStart)
                               {
                                 return self["collapse"](toStart);
                               });
var h$webkit_dom_range_select_node;
h$webkit_dom_range_select_node = (function(self,
                                  self_2, refNode, refNode_2)
                                  {
                                    return self["selectNode"](refNode);
                                  });
var h$webkit_dom_range_select_node_contents;
h$webkit_dom_range_select_node_contents = (function(self,
                                           self_2, refNode, refNode_2)
                                           {
                                             return self["selectNodeContents"](refNode);
                                           });
var h$webkit_dom_range_compare_boundary_points;
h$webkit_dom_range_compare_boundary_points = (function(self,
                                              self_2, how, sourceRange,
                                              sourceRange_2)
                                              {
                                                return self["compareBoundaryPoints"](how,
                                                sourceRange);
                                              });
var h$webkit_dom_range_delete_contents;
h$webkit_dom_range_delete_contents = (function(self,
                                      self_2)
                                      {
                                        return self["deleteContents"]();
                                      });
var h$webkit_dom_range_extract_contents;
h$webkit_dom_range_extract_contents = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["extractContents"]();
                                       });
var h$webkit_dom_range_clone_contents;
h$webkit_dom_range_clone_contents = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["cloneContents"]();
                                     });
var h$webkit_dom_range_insert_node;
h$webkit_dom_range_insert_node = (function(self,
                                  self_2, newNode, newNode_2)
                                  {
                                    return self["insertNode"](newNode);
                                  });
var h$webkit_dom_range_surround_contents;
h$webkit_dom_range_surround_contents = (function(self,
                                        self_2, newParent, newParent_2)
                                        {
                                          return self["surroundContents"](newParent);
                                        });
var h$webkit_dom_range_clone_range;
h$webkit_dom_range_clone_range = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return self["cloneRange"]();
                                  });
var h$webkit_dom_range_to_string;
h$webkit_dom_range_to_string = (function(self,
                                self_2)
                                {
                                  h$ret1 = 0;
                                  return h$encodeUtf8(self["toString"]());
                                });
var h$webkit_dom_range_detach;
h$webkit_dom_range_detach = (function(self,
                             self_2)
                             {
                               return self["detach"]();
                             });
var h$webkit_dom_range_create_contextual_fragment;
h$webkit_dom_range_create_contextual_fragment = (function(self,
                                                 self_2, html, html_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["createContextualFragment"](h$decodeUtf8z(html,
                                                   html_2));
                                                 });
var h$webkit_dom_range_intersects_node;
h$webkit_dom_range_intersects_node = (function(self,
                                      self_2, refNode, refNode_2)
                                      {
                                        return self["intersectsNode"](refNode);
                                      });
var h$webkit_dom_range_compare_node;
h$webkit_dom_range_compare_node = (function(self,
                                   self_2, refNode, refNode_2)
                                   {
                                     return self["compareNode"](refNode);
                                   });
var h$webkit_dom_range_compare_point;
h$webkit_dom_range_compare_point = (function(self,
                                    self_2, refNode, refNode_2,
                                    offset)
                                    {
                                      return self["comparePoint"](refNode,
                                      offset);
                                    });
var h$webkit_dom_range_is_point_in_range;
h$webkit_dom_range_is_point_in_range = (function(self,
                                        self_2, refNode, refNode_2,
                                        offset)
                                        {
                                          return self["isPointInRange"](refNode,
                                          offset);
                                        });
var h$webkit_dom_range_expand;
h$webkit_dom_range_expand = (function(self,
                             self_2, unit, unit_2)
                             {
                               return self["expand"](h$decodeUtf8z(unit,
                               unit_2));
                             });
var h$webkit_dom_range_get_start_container;
h$webkit_dom_range_get_start_container = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["startContainer"];
                                          });
var h$webkit_dom_range_get_start_offset;
h$webkit_dom_range_get_start_offset = (function(self,
                                       self_2)
                                       {
                                         return self["startOffset"];
                                       });
var h$webkit_dom_range_get_end_container;
h$webkit_dom_range_get_end_container = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["endContainer"];
                                        });
var h$webkit_dom_range_get_end_offset;
h$webkit_dom_range_get_end_offset = (function(self,
                                     self_2)
                                     {
                                       return self["endOffset"];
                                     });
var h$webkit_dom_range_get_collapsed;
h$webkit_dom_range_get_collapsed = (function(self,
                                    self_2)
                                    {
                                      return self["collapsed"];
                                    });
var h$webkit_dom_range_get_common_ancestor_container;
h$webkit_dom_range_get_common_ancestor_container = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["commonAncestorContainer"];
                                                    });
var h$webkit_dom_range_get_text;
h$webkit_dom_range_get_text = (function(self,
                               self_2)
                               {
                                 h$ret1 = 0;
                                 return h$encodeUtf8(self["text"]);
                               });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_processing_instruction_get_type = (function()
                                                {
                                                  return h$g_get_type(ProcessingInstruction);
                                                });
var h$webkit_dom_processing_instruction_get_target;
h$webkit_dom_processing_instruction_get_target = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["target"]);
                                                  });
var h$webkit_dom_processing_instruction_set_data;
h$webkit_dom_processing_instruction_set_data = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["data"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_processing_instruction_get_data;
h$webkit_dom_processing_instruction_get_data = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["data"]);
                                                });
var h$webkit_dom_processing_instruction_get_sheet;
h$webkit_dom_processing_instruction_get_sheet = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["sheet"];
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_notation_get_type = (function()
                                  {
                                    return h$g_get_type(Notation);
                                  });
var h$webkit_dom_notation_get_public_id;
h$webkit_dom_notation_get_public_id = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["publicId"]);
                                       });
var h$webkit_dom_notation_get_system_id;
h$webkit_dom_notation_get_system_id = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["systemId"]);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_node_list_get_type = (function()
                                   {
                                     return h$g_get_type(NodeList);
                                   });
var h$webkit_dom_node_list_item;
h$webkit_dom_node_list_item = (function(self,
                               self_2, index)
                               {
                                 h$ret1 = 0;
                                 return self["item"](index);
                               });
var h$webkit_dom_node_list_get_length;
h$webkit_dom_node_list_get_length = (function(self,
                                     self_2)
                                     {
                                       return self["length"];
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Traversal
h$webkit_dom_node_iterator_get_type = (function()
                                       {
                                         return h$g_get_type(NodeIterator);
                                       });
var h$webkit_dom_node_iterator_detach;
h$webkit_dom_node_iterator_detach = (function(self,
                                     self_2)
                                     {
                                       return self["detach"]();
                                     });
var h$webkit_dom_node_iterator_get_root;
h$webkit_dom_node_iterator_get_root = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["root"];
                                       });
var h$webkit_dom_node_iterator_get_what_to_show;
h$webkit_dom_node_iterator_get_what_to_show = (function(self,
                                               self_2)
                                               {
                                                 return self["whatToShow"];
                                               });
var h$webkit_dom_node_iterator_get_filter;
h$webkit_dom_node_iterator_get_filter = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["filter"];
                                         });
var h$webkit_dom_node_iterator_get_expand_entity_references;
h$webkit_dom_node_iterator_get_expand_entity_references = (function(self,
                                                           self_2)
                                                           {
                                                             return self["expandEntityReferences"];
                                                           });
var h$webkit_dom_node_iterator_get_reference_node;
h$webkit_dom_node_iterator_get_reference_node = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["referenceNode"];
                                                 });
var h$webkit_dom_node_iterator_get_pointer_before_reference_node;
h$webkit_dom_node_iterator_get_pointer_before_reference_node = (function(self,
                                                                self_2)
                                                                {
                                                                  return self["pointerBeforeReferenceNode"];
                                                                });
// Graphics.UI.Gtk.WebKit.DOM.Traversal
h$webkit_dom_node_filter_get_type = (function()
                                     {
                                       return h$g_get_type(NodeFilter);
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_node_get_type = (function()
                              {
                                return h$g_get_type(Node);
                              });
var h$webkit_dom_node_insert_before;
h$webkit_dom_node_insert_before = (function(self,
                                   self_2, newChild, newChild_2,
                                   refChild, refChild_2)
                                   {
                                     h$ret1 = 0;
                                     return self["insertBefore"](newChild,
                                     refChild);
                                   });
var h$webkit_dom_node_replace_child;
h$webkit_dom_node_replace_child = (function(self,
                                   self_2, newChild, newChild_2,
                                   oldChild, oldChild_2)
                                   {
                                     h$ret1 = 0;
                                     return self["replaceChild"](newChild,
                                     oldChild);
                                   });
var h$webkit_dom_node_remove_child;
h$webkit_dom_node_remove_child = (function(self,
                                  self_2, oldChild, oldChild_2)
                                  {
                                    h$ret1 = 0;
                                    return self["removeChild"](oldChild);
                                  });
var h$webkit_dom_node_append_child;
h$webkit_dom_node_append_child = (function(self,
                                  self_2, newChild, newChild_2)
                                  {
                                    h$ret1 = 0;
                                    return self["appendChild"](newChild);
                                  });
var h$webkit_dom_node_has_child_nodes;
h$webkit_dom_node_has_child_nodes = (function(self,
                                     self_2)
                                     {
                                       return self["hasChildNodes"]();
                                     });
var h$webkit_dom_node_clone_node;
h$webkit_dom_node_clone_node = (function(self,
                                self_2, deep)
                                {
                                  h$ret1 = 0;
                                  return self["cloneNode"](deep);
                                });
var h$webkit_dom_node_normalize;
h$webkit_dom_node_normalize = (function(self,
                               self_2)
                               {
                                 return self["normalize"]();
                               });
var h$webkit_dom_node_is_supported;
h$webkit_dom_node_is_supported = (function(self,
                                  self_2, feature, feature_2,
                                  version, version_2)
                                  {
                                    return self["isSupported"](h$decodeUtf8z(feature,
                                    feature_2),
                                    h$decodeUtf8z(version,
                                    version_2));
                                  });
var h$webkit_dom_node_has_attributes;
h$webkit_dom_node_has_attributes = (function(self,
                                    self_2)
                                    {
                                      return self["hasAttributes"]();
                                    });
var h$webkit_dom_node_is_same_node;
h$webkit_dom_node_is_same_node = (function(self,
                                  self_2, other, other_2)
                                  {
                                    return self["isSameNode"](other);
                                  });
var h$webkit_dom_node_is_equal_node;
h$webkit_dom_node_is_equal_node = (function(self,
                                   self_2, other, other_2)
                                   {
                                     return self["isEqualNode"](other);
                                   });
var h$webkit_dom_node_lookup_prefix;
h$webkit_dom_node_lookup_prefix = (function(self,
                                   self_2, namespaceURI,
                                   namespaceURI_2)
                                   {
                                     h$ret1 = 0;
                                     return h$encodeUtf8(self["lookupPrefix"](h$decodeUtf8z(namespaceURI,
                                     namespaceURI_2)));
                                   });
var h$webkit_dom_node_is_default_namespace;
h$webkit_dom_node_is_default_namespace = (function(self,
                                          self_2, namespaceURI,
                                          namespaceURI_2)
                                          {
                                            return self["isDefaultNamespace"](h$decodeUtf8z(namespaceURI,
                                            namespaceURI_2));
                                          });
var h$webkit_dom_node_lookup_namespace_uri;
h$webkit_dom_node_lookup_namespace_uri = (function(self,
                                          self_2, prefix, prefix_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["lookupNamespaceURI"](h$decodeUtf8z(prefix,
                                            prefix_2)));
                                          });
var h$webkit_dom_node_compare_document_position;
h$webkit_dom_node_compare_document_position = (function(self,
                                               self_2, other, other_2)
                                               {
                                                 return self["compareDocumentPosition"](other);
                                               });
var h$webkit_dom_node_contains;
h$webkit_dom_node_contains = (function(self,
                              self_2, other, other_2)
                              {
                                return self["contains"](other);
                              });
var h$webkit_dom_node_dispatch_event;
h$webkit_dom_node_dispatch_event = (function(self,
                                    self_2, event, event_2)
                                    {
                                      return self["dispatchEvent"](event);
                                    });
var h$webkit_dom_node_get_node_name;
h$webkit_dom_node_get_node_name = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return h$encodeUtf8(self["nodeName"]);
                                   });
var h$webkit_dom_node_set_node_value;
h$webkit_dom_node_set_node_value = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["nodeValue"] = h$decodeUtf8z(val,
                                      val_2);
                                    });
var h$webkit_dom_node_get_node_value;
h$webkit_dom_node_get_node_value = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["nodeValue"]);
                                    });
var h$webkit_dom_node_get_node_type;
h$webkit_dom_node_get_node_type = (function(self,
                                   self_2)
                                   {
                                     return self["nodeType"];
                                   });
var h$webkit_dom_node_get_parent_node;
h$webkit_dom_node_get_parent_node = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["parentNode"];
                                     });
var h$webkit_dom_node_get_child_nodes;
h$webkit_dom_node_get_child_nodes = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["childNodes"];
                                     });
var h$webkit_dom_node_get_first_child;
h$webkit_dom_node_get_first_child = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["firstChild"];
                                     });
var h$webkit_dom_node_get_last_child;
h$webkit_dom_node_get_last_child = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["lastChild"];
                                    });
var h$webkit_dom_node_get_previous_sibling;
h$webkit_dom_node_get_previous_sibling = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["previousSibling"];
                                          });
var h$webkit_dom_node_get_next_sibling;
h$webkit_dom_node_get_next_sibling = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["nextSibling"];
                                      });
var h$webkit_dom_node_get_attributes;
h$webkit_dom_node_get_attributes = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["attributes"];
                                    });
var h$webkit_dom_node_get_owner_document;
h$webkit_dom_node_get_owner_document = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ownerDocument"];
                                        });
var h$webkit_dom_node_get_namespace_uri;
h$webkit_dom_node_get_namespace_uri = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["namespaceURI"]);
                                       });
var h$webkit_dom_node_set_prefix;
h$webkit_dom_node_set_prefix = (function(self,
                                self_2, val, val_2)
                                {
                                  self["prefix"] = h$decodeUtf8z(val,
                                  val_2);
                                });
var h$webkit_dom_node_get_prefix;
h$webkit_dom_node_get_prefix = (function(self,
                                self_2)
                                {
                                  h$ret1 = 0;
                                  return h$encodeUtf8(self["prefix"]);
                                });
var h$webkit_dom_node_get_local_name;
h$webkit_dom_node_get_local_name = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["localName"]);
                                    });
var h$webkit_dom_node_get_base_uri;
h$webkit_dom_node_get_base_uri = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return h$encodeUtf8(self["baseURI"]);
                                  });
var h$webkit_dom_node_set_text_content;
h$webkit_dom_node_set_text_content = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["textContent"] = h$decodeUtf8z(val,
                                        val_2);
                                      });
var h$webkit_dom_node_get_text_content;
h$webkit_dom_node_get_text_content = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["textContent"]);
                                      });
var h$webkit_dom_node_get_parent_element;
h$webkit_dom_node_get_parent_element = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["parentElement"];
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_navigator_get_type = (function()
                                   {
                                     return h$g_get_type(Navigator);
                                   });
var h$webkit_dom_navigator_java_enabled;
h$webkit_dom_navigator_java_enabled = (function(self,
                                       self_2)
                                       {
                                         return self["javaEnabled"]();
                                       });
var h$webkit_dom_navigator_get_storage_updates;
h$webkit_dom_navigator_get_storage_updates = (function(self,
                                              self_2)
                                              {
                                                return self["getStorageUpdates"]();
                                              });
var h$webkit_dom_navigator_get_app_code_name;
h$webkit_dom_navigator_get_app_code_name = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["appCodeName"]);
                                            });
var h$webkit_dom_navigator_get_app_name;
h$webkit_dom_navigator_get_app_name = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["appName"]);
                                       });
var h$webkit_dom_navigator_get_app_version;
h$webkit_dom_navigator_get_app_version = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["appVersion"]);
                                          });
var h$webkit_dom_navigator_get_language;
h$webkit_dom_navigator_get_language = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["language"]);
                                       });
var h$webkit_dom_navigator_get_user_agent;
h$webkit_dom_navigator_get_user_agent = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["userAgent"]);
                                         });
var h$webkit_dom_navigator_get_platform;
h$webkit_dom_navigator_get_platform = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["platform"]);
                                       });
var h$webkit_dom_navigator_get_plugins;
h$webkit_dom_navigator_get_plugins = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["plugins"];
                                      });
var h$webkit_dom_navigator_get_mime_types;
h$webkit_dom_navigator_get_mime_types = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["mimeTypes"];
                                         });
var h$webkit_dom_navigator_get_product;
h$webkit_dom_navigator_get_product = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["product"]);
                                      });
var h$webkit_dom_navigator_get_product_sub;
h$webkit_dom_navigator_get_product_sub = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["productSub"]);
                                          });
var h$webkit_dom_navigator_get_vendor;
h$webkit_dom_navigator_get_vendor = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return h$encodeUtf8(self["vendor"]);
                                     });
var h$webkit_dom_navigator_get_vendor_sub;
h$webkit_dom_navigator_get_vendor_sub = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["vendorSub"]);
                                         });
var h$webkit_dom_navigator_get_cookie_enabled;
h$webkit_dom_navigator_get_cookie_enabled = (function(self,
                                             self_2)
                                             {
                                               return self["cookieEnabled"];
                                             });
var h$webkit_dom_navigator_get_on_line;
h$webkit_dom_navigator_get_on_line = (function(self,
                                      self_2)
                                      {
                                        return self["onLine"];
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_named_node_map_get_type = (function()
                                        {
                                          return h$g_get_type(NamedNodeMap);
                                        });
var h$webkit_dom_named_node_map_get_named_item;
h$webkit_dom_named_node_map_get_named_item = (function(self,
                                              self_2, name, name_2)
                                              {
                                                h$ret1 = 0;
                                                return self["getNamedItem"](h$decodeUtf8z(name,
                                                name_2));
                                              });
var h$webkit_dom_named_node_map_set_named_item;
h$webkit_dom_named_node_map_set_named_item = (function(self,
                                              self_2, node, node_2)
                                              {
                                                h$ret1 = 0;
                                                return self["setNamedItem"](node);
                                              });
var h$webkit_dom_named_node_map_remove_named_item;
h$webkit_dom_named_node_map_remove_named_item = (function(self,
                                                 self_2, name, name_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["removeNamedItem"](h$decodeUtf8z(name,
                                                   name_2));
                                                 });
var h$webkit_dom_named_node_map_item;
h$webkit_dom_named_node_map_item = (function(self,
                                    self_2, index)
                                    {
                                      h$ret1 = 0;
                                      return self["item"](index);
                                    });
var h$webkit_dom_named_node_map_get_named_item_ns;
h$webkit_dom_named_node_map_get_named_item_ns = (function(self,
                                                 self_2, namespaceURI,
                                                 namespaceURI_2, localName,
                                                 localName_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["getNamedItemNS"](h$decodeUtf8z(namespaceURI,
                                                   namespaceURI_2),
                                                   h$decodeUtf8z(localName,
                                                   localName_2));
                                                 });
var h$webkit_dom_named_node_map_set_named_item_ns;
h$webkit_dom_named_node_map_set_named_item_ns = (function(self,
                                                 self_2, node, node_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["setNamedItemNS"](node);
                                                 });
var h$webkit_dom_named_node_map_remove_named_item_ns;
h$webkit_dom_named_node_map_remove_named_item_ns = (function(self,
                                                    self_2, namespaceURI,
                                                    namespaceURI_2, localName,
                                                    localName_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["removeNamedItemNS"](h$decodeUtf8z(namespaceURI,
                                                      namespaceURI_2),
                                                      h$decodeUtf8z(localName,
                                                      localName_2));
                                                    });
var h$webkit_dom_named_node_map_get_length;
h$webkit_dom_named_node_map_get_length = (function(self,
                                          self_2)
                                          {
                                            return self["length"];
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_mutation_event_get_type = (function()
                                        {
                                          return h$g_get_type(MutationEvent);
                                        });
var h$webkit_dom_mutation_event_init_mutation_event;
h$webkit_dom_mutation_event_init_mutation_event = (function(self,
                                                   self_2, type, type_2,
                                                   canBubble, cancelable,
                                                   relatedNode, relatedNode_2,
                                                   prevValue, prevValue_2,
                                                   newValue, newValue_2,
                                                   attrName, attrName_2,
                                                   attrChange)
                                                   {
                                                     return self["initMutationEvent"](h$decodeUtf8z(type,
                                                     type_2), canBubble,
                                                     cancelable, relatedNode,
                                                     h$decodeUtf8z(prevValue,
                                                     prevValue_2),
                                                     h$decodeUtf8z(newValue,
                                                     newValue_2),
                                                     h$decodeUtf8z(attrName,
                                                     attrName_2), attrChange);
                                                   });
var h$webkit_dom_mutation_event_get_related_node;
h$webkit_dom_mutation_event_get_related_node = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["relatedNode"];
                                                });
var h$webkit_dom_mutation_event_get_prev_value;
h$webkit_dom_mutation_event_get_prev_value = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["prevValue"]);
                                              });
var h$webkit_dom_mutation_event_get_new_value;
h$webkit_dom_mutation_event_get_new_value = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["newValue"]);
                                             });
var h$webkit_dom_mutation_event_get_attr_name;
h$webkit_dom_mutation_event_get_attr_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["attrName"]);
                                             });
var h$webkit_dom_mutation_event_get_attr_change;
h$webkit_dom_mutation_event_get_attr_change = (function(self,
                                               self_2)
                                               {
                                                 return self["attrChange"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_message_port_get_type = (function()
                                      {
                                        return h$g_get_type(MessagePort);
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_mouse_event_get_type = (function()
                                     {
                                       return h$g_get_type(MouseEvent);
                                     });
var h$webkit_dom_mouse_event_init_mouse_event;
h$webkit_dom_mouse_event_init_mouse_event = (function(self,
                                             self_2, type, type_2, canBubble,
                                             cancelable, view, view_2,
                                             detail, screenX, screenY,
                                             clientX, clientY, ctrlKey,
                                             altKey, shiftKey, metaKey,
                                             button, relatedTarget,
                                             relatedTarget_2)
                                             {
                                               return self["initMouseEvent"](h$decodeUtf8z(type,
                                               type_2), canBubble, cancelable,
                                               view, detail, screenX, screenY,
                                               clientX, clientY, ctrlKey,
                                               altKey, shiftKey, metaKey,
                                               button, relatedTarget);
                                             });
var h$webkit_dom_mouse_event_get_screen_x;
h$webkit_dom_mouse_event_get_screen_x = (function(self,
                                         self_2)
                                         {
                                           return self["screenX"];
                                         });
var h$webkit_dom_mouse_event_get_screen_y;
h$webkit_dom_mouse_event_get_screen_y = (function(self,
                                         self_2)
                                         {
                                           return self["screenY"];
                                         });
var h$webkit_dom_mouse_event_get_client_x;
h$webkit_dom_mouse_event_get_client_x = (function(self,
                                         self_2)
                                         {
                                           return self["clientX"];
                                         });
var h$webkit_dom_mouse_event_get_client_y;
h$webkit_dom_mouse_event_get_client_y = (function(self,
                                         self_2)
                                         {
                                           return self["clientY"];
                                         });
var h$webkit_dom_mouse_event_get_webkit_movement_x;
h$webkit_dom_mouse_event_get_webkit_movement_x = (function(self,
                                                  self_2)
                                                  {
                                                    return self["webkitMovementX"];
                                                  });
var h$webkit_dom_mouse_event_get_webkit_movement_y;
h$webkit_dom_mouse_event_get_webkit_movement_y = (function(self,
                                                  self_2)
                                                  {
                                                    return self["webkitMovementY"];
                                                  });
var h$webkit_dom_mouse_event_get_ctrl_key;
h$webkit_dom_mouse_event_get_ctrl_key = (function(self,
                                         self_2)
                                         {
                                           return self["ctrlKey"];
                                         });
var h$webkit_dom_mouse_event_get_shift_key;
h$webkit_dom_mouse_event_get_shift_key = (function(self,
                                          self_2)
                                          {
                                            return self["shiftKey"];
                                          });
var h$webkit_dom_mouse_event_get_alt_key;
h$webkit_dom_mouse_event_get_alt_key = (function(self,
                                        self_2)
                                        {
                                          return self["altKey"];
                                        });
var h$webkit_dom_mouse_event_get_meta_key;
h$webkit_dom_mouse_event_get_meta_key = (function(self,
                                         self_2)
                                         {
                                           return self["metaKey"];
                                         });
var h$webkit_dom_mouse_event_get_button;
h$webkit_dom_mouse_event_get_button = (function(self,
                                       self_2)
                                       {
                                         return self["button"];
                                       });
var h$webkit_dom_mouse_event_get_related_target;
h$webkit_dom_mouse_event_get_related_target = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["relatedTarget"];
                                               });
var h$webkit_dom_mouse_event_get_offset_x;
h$webkit_dom_mouse_event_get_offset_x = (function(self,
                                         self_2)
                                         {
                                           return self["offsetX"];
                                         });
var h$webkit_dom_mouse_event_get_offset_y;
h$webkit_dom_mouse_event_get_offset_y = (function(self,
                                         self_2)
                                         {
                                           return self["offsetY"];
                                         });
var h$webkit_dom_mouse_event_get_x;
h$webkit_dom_mouse_event_get_x = (function(self,
                                  self_2)
                                  {
                                    return self["x"];
                                  });
var h$webkit_dom_mouse_event_get_y;
h$webkit_dom_mouse_event_get_y = (function(self,
                                  self_2)
                                  {
                                    return self["y"];
                                  });
var h$webkit_dom_mouse_event_get_from_element;
h$webkit_dom_mouse_event_get_from_element = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["fromElement"];
                                             });
var h$webkit_dom_mouse_event_get_to_element;
h$webkit_dom_mouse_event_get_to_element = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["toElement"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_memory_info_get_type = (function()
                                     {
                                       return h$g_get_type(MemoryInfo);
                                     });
var h$webkit_dom_memory_info_get_total_js_heap_size;
h$webkit_dom_memory_info_get_total_js_heap_size = (function(self,
                                                   self_2)
                                                   {
                                                     return self["totalJSHeapSize"];
                                                   });
var h$webkit_dom_memory_info_get_used_js_heap_size;
h$webkit_dom_memory_info_get_used_js_heap_size = (function(self,
                                                  self_2)
                                                  {
                                                    return self["usedJSHeapSize"];
                                                  });
var h$webkit_dom_memory_info_get_js_heap_size_limit;
h$webkit_dom_memory_info_get_js_heap_size_limit = (function(self,
                                                   self_2)
                                                   {
                                                     return self["jsHeapSizeLimit"];
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.View
h$webkit_dom_media_query_list_get_type = (function()
                                          {
                                            return h$g_get_type(MediaQueryList);
                                          });
var h$webkit_dom_media_query_list_get_media;
h$webkit_dom_media_query_list_get_media = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["media"]);
                                           });
var h$webkit_dom_media_query_list_get_matches;
h$webkit_dom_media_query_list_get_matches = (function(self,
                                             self_2)
                                             {
                                               return self["matches"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Stylesheets
h$webkit_dom_media_list_get_type = (function()
                                    {
                                      return h$g_get_type(MediaList);
                                    });
var h$webkit_dom_media_list_item;
h$webkit_dom_media_list_item = (function(self,
                                self_2, index)
                                {
                                  h$ret1 = 0;
                                  return h$encodeUtf8(self["item"](index));
                                });
var h$webkit_dom_media_list_delete_medium;
h$webkit_dom_media_list_delete_medium = (function(self,
                                         self_2, oldMedium, oldMedium_2)
                                         {
                                           return self["deleteMedium"](h$decodeUtf8z(oldMedium,
                                           oldMedium_2));
                                         });
var h$webkit_dom_media_list_append_medium;
h$webkit_dom_media_list_append_medium = (function(self,
                                         self_2, newMedium, newMedium_2)
                                         {
                                           return self["appendMedium"](h$decodeUtf8z(newMedium,
                                           newMedium_2));
                                         });
var h$webkit_dom_media_list_set_media_text;
h$webkit_dom_media_list_set_media_text = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["mediaText"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_media_list_get_media_text;
h$webkit_dom_media_list_get_media_text = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["mediaText"]);
                                          });
var h$webkit_dom_media_list_get_length;
h$webkit_dom_media_list_get_length = (function(self,
                                      self_2)
                                      {
                                        return self["length"];
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_media_error_get_type = (function()
                                     {
                                       return h$g_get_type(MediaError);
                                     });
var h$webkit_dom_media_error_get_code;
h$webkit_dom_media_error_get_code = (function(self,
                                     self_2)
                                     {
                                       return self["code"];
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_location_get_type = (function()
                                  {
                                    return h$g_get_type(Location);
                                  });
var h$webkit_dom_location_get_origin;
h$webkit_dom_location_get_origin = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["origin"]);
                                    });
var h$webkit_dom_location_get_ancestor_origins;
h$webkit_dom_location_get_ancestor_origins = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["ancestorOrigins"];
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_keyboard_event_get_type = (function()
                                        {
                                          return h$g_get_type(KeyboardEvent);
                                        });
var h$webkit_dom_keyboard_event_get_modifier_state;
h$webkit_dom_keyboard_event_get_modifier_state = (function(self,
                                                  self_2, keyIdentifierArg,
                                                  keyIdentifierArg_2)
                                                  {
                                                    return self["getModifierState"](h$decodeUtf8z(keyIdentifierArg,
                                                    keyIdentifierArg_2));
                                                  });
var h$webkit_dom_keyboard_event_init_keyboard_event;
h$webkit_dom_keyboard_event_init_keyboard_event = (function(self,
                                                   self_2, type, type_2,
                                                   canBubble, cancelable, view,
                                                   view_2, keyIdentifier,
                                                   keyIdentifier_2, keyLocation,
                                                   ctrlKey, altKey, shiftKey,
                                                   metaKey, altGraphKey)
                                                   {
                                                     return self["initKeyboardEvent"](h$decodeUtf8z(type,
                                                     type_2), canBubble,
                                                     cancelable, view,
                                                     h$decodeUtf8z(keyIdentifier,
                                                     keyIdentifier_2),
                                                     keyLocation, ctrlKey,
                                                     altKey, shiftKey, metaKey,
                                                     altGraphKey);
                                                   });
var h$webkit_dom_keyboard_event_init_keyboard_event;
h$webkit_dom_keyboard_event_init_keyboard_event = (function(self,
                                                   self_2, type, type_2,
                                                   canBubble, cancelable, view,
                                                   view_2, keyIdentifier,
                                                   keyIdentifier_2, keyLocation,
                                                   ctrlKey, altKey, shiftKey,
                                                   metaKey)
                                                   {
                                                     return self["initKeyboardEvent"](h$decodeUtf8z(type,
                                                     type_2), canBubble,
                                                     cancelable, view,
                                                     h$decodeUtf8z(keyIdentifier,
                                                     keyIdentifier_2),
                                                     keyLocation, ctrlKey,
                                                     altKey, shiftKey, metaKey);
                                                   });
var h$webkit_dom_keyboard_event_get_key_identifier;
h$webkit_dom_keyboard_event_get_key_identifier = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["keyIdentifier"]);
                                                  });
var h$webkit_dom_keyboard_event_get_key_location;
h$webkit_dom_keyboard_event_get_key_location = (function(self,
                                                self_2)
                                                {
                                                  return self["keyLocation"];
                                                });
var h$webkit_dom_keyboard_event_get_ctrl_key;
h$webkit_dom_keyboard_event_get_ctrl_key = (function(self,
                                            self_2)
                                            {
                                              return self["ctrlKey"];
                                            });
var h$webkit_dom_keyboard_event_get_shift_key;
h$webkit_dom_keyboard_event_get_shift_key = (function(self,
                                             self_2)
                                             {
                                               return self["shiftKey"];
                                             });
var h$webkit_dom_keyboard_event_get_alt_key;
h$webkit_dom_keyboard_event_get_alt_key = (function(self,
                                           self_2)
                                           {
                                             return self["altKey"];
                                           });
var h$webkit_dom_keyboard_event_get_meta_key;
h$webkit_dom_keyboard_event_get_meta_key = (function(self,
                                            self_2)
                                            {
                                              return self["metaKey"];
                                            });
var h$webkit_dom_keyboard_event_get_alt_graph_key;
h$webkit_dom_keyboard_event_get_alt_graph_key = (function(self,
                                                 self_2)
                                                 {
                                                   return self["altGraphKey"];
                                                 });
var h$webkit_dom_keyboard_event_get_key_code;
h$webkit_dom_keyboard_event_get_key_code = (function(self,
                                            self_2)
                                            {
                                              return self["keyCode"];
                                            });
var h$webkit_dom_keyboard_event_get_char_code;
h$webkit_dom_keyboard_event_get_char_code = (function(self,
                                             self_2)
                                             {
                                               return self["charCode"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_video_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLVideoElement);
                                            });
var h$webkit_dom_html_video_element_webkit_enter_fullscreen;
h$webkit_dom_html_video_element_webkit_enter_fullscreen = (function(self,
                                                           self_2)
                                                           {
                                                             return self["webkitEnterFullscreen"]();
                                                           });
var h$webkit_dom_html_video_element_webkit_exit_fullscreen;
h$webkit_dom_html_video_element_webkit_exit_fullscreen = (function(self,
                                                          self_2)
                                                          {
                                                            return self["webkitExitFullscreen"]();
                                                          });
var h$webkit_dom_html_video_element_webkit_enter_full_screen;
h$webkit_dom_html_video_element_webkit_enter_full_screen = (function(self,
                                                            self_2)
                                                            {
                                                              return self["webkitEnterFullScreen"]();
                                                            });
var h$webkit_dom_html_video_element_webkit_exit_full_screen;
h$webkit_dom_html_video_element_webkit_exit_full_screen = (function(self,
                                                           self_2)
                                                           {
                                                             return self["webkitExitFullScreen"]();
                                                           });
var h$webkit_dom_html_video_element_set_width;
h$webkit_dom_html_video_element_set_width = (function(self,
                                             self_2, val)
                                             {
                                               self["width"] = val;
                                             });
var h$webkit_dom_html_video_element_get_width;
h$webkit_dom_html_video_element_get_width = (function(self,
                                             self_2)
                                             {
                                               return self["width"];
                                             });
var h$webkit_dom_html_video_element_set_height;
h$webkit_dom_html_video_element_set_height = (function(self,
                                              self_2, val)
                                              {
                                                self["height"] = val;
                                              });
var h$webkit_dom_html_video_element_get_height;
h$webkit_dom_html_video_element_get_height = (function(self,
                                              self_2)
                                              {
                                                return self["height"];
                                              });
var h$webkit_dom_html_video_element_get_video_width;
h$webkit_dom_html_video_element_get_video_width = (function(self,
                                                   self_2)
                                                   {
                                                     return self["videoWidth"];
                                                   });
var h$webkit_dom_html_video_element_get_video_height;
h$webkit_dom_html_video_element_get_video_height = (function(self,
                                                    self_2)
                                                    {
                                                      return self["videoHeight"];
                                                    });
var h$webkit_dom_html_video_element_set_poster;
h$webkit_dom_html_video_element_set_poster = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["poster"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_video_element_get_poster;
h$webkit_dom_html_video_element_get_poster = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["poster"]);
                                              });
var h$webkit_dom_html_video_element_get_webkit_supports_fullscreen;
h$webkit_dom_html_video_element_get_webkit_supports_fullscreen = (function(self,
                                                                  self_2)
                                                                  {
                                                                    return self["webkitSupportsFullscreen"];
                                                                  });
var h$webkit_dom_html_video_element_get_webkit_displaying_fullscreen;
h$webkit_dom_html_video_element_get_webkit_displaying_fullscreen = (function(self,
                                                                    self_2)
                                                                    {
                                                                      return self["webkitDisplayingFullscreen"];
                                                                    });
var h$webkit_dom_html_video_element_get_webkit_decoded_frame_count;
h$webkit_dom_html_video_element_get_webkit_decoded_frame_count = (function(self,
                                                                  self_2)
                                                                  {
                                                                    return self["webkitDecodedFrameCount"];
                                                                  });
var h$webkit_dom_html_video_element_get_webkit_dropped_frame_count;
h$webkit_dom_html_video_element_get_webkit_dropped_frame_count = (function(self,
                                                                  self_2)
                                                                  {
                                                                    return self["webkitDroppedFrameCount"];
                                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_htmlu_list_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLUListElement);
                                            });
var h$webkit_dom_htmlu_list_element_set_compact;
h$webkit_dom_htmlu_list_element_set_compact = (function(self,
                                               self_2, val)
                                               {
                                                 self["compact"] = val;
                                               });
var h$webkit_dom_htmlu_list_element_get_compact;
h$webkit_dom_htmlu_list_element_get_compact = (function(self,
                                               self_2)
                                               {
                                                 return self["compact"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_title_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLTitleElement);
                                            });
var h$webkit_dom_html_title_element_set_text;
h$webkit_dom_html_title_element_set_text = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["text"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_title_element_get_text;
h$webkit_dom_html_title_element_get_text = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["text"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_text_area_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLTextAreaElement);
                                                });
var h$webkit_dom_html_text_area_element_check_validity;
h$webkit_dom_html_text_area_element_check_validity = (function(self,
                                                      self_2)
                                                      {
                                                        return self["checkValidity"]();
                                                      });
var h$webkit_dom_html_text_area_element_set_custom_validity;
h$webkit_dom_html_text_area_element_set_custom_validity = (function(self,
                                                           self_2, error,
                                                           error_2)
                                                           {
                                                             return self["setCustomValidity"](h$decodeUtf8z(error,
                                                             error_2));
                                                           });
var h$webkit_dom_html_text_area_element_select;
h$webkit_dom_html_text_area_element_select = (function(self,
                                              self_2)
                                              {
                                                return self["select"]();
                                              });
var h$webkit_dom_html_text_area_element_set_selection_range;
h$webkit_dom_html_text_area_element_set_selection_range = (function(self,
                                                           self_2, start, end,
                                                           direction,
                                                           direction_2)
                                                           {
                                                             return self["setSelectionRange"](start,
                                                             end,
                                                             h$decodeUtf8z(direction,
                                                             direction_2));
                                                           });
var h$webkit_dom_html_text_area_element_set_autofocus;
h$webkit_dom_html_text_area_element_set_autofocus = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["autofocus"] = val;
                                                     });
var h$webkit_dom_html_text_area_element_get_autofocus;
h$webkit_dom_html_text_area_element_get_autofocus = (function(self,
                                                     self_2)
                                                     {
                                                       return self["autofocus"];
                                                     });
var h$webkit_dom_html_text_area_element_set_cols;
h$webkit_dom_html_text_area_element_set_cols = (function(self,
                                                self_2, val)
                                                {
                                                  self["cols"] = val;
                                                });
var h$webkit_dom_html_text_area_element_get_cols;
h$webkit_dom_html_text_area_element_get_cols = (function(self,
                                                self_2)
                                                {
                                                  return self["cols"];
                                                });
var h$webkit_dom_html_text_area_element_set_dir_name;
h$webkit_dom_html_text_area_element_set_dir_name = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["dirName"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_text_area_element_get_dir_name;
h$webkit_dom_html_text_area_element_get_dir_name = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["dirName"]);
                                                    });
var h$webkit_dom_html_text_area_element_set_disabled;
h$webkit_dom_html_text_area_element_set_disabled = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["disabled"] = val;
                                                    });
var h$webkit_dom_html_text_area_element_get_disabled;
h$webkit_dom_html_text_area_element_get_disabled = (function(self,
                                                    self_2)
                                                    {
                                                      return self["disabled"];
                                                    });
var h$webkit_dom_html_text_area_element_get_form;
h$webkit_dom_html_text_area_element_get_form = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["form"];
                                                });
var h$webkit_dom_html_text_area_element_set_max_length;
h$webkit_dom_html_text_area_element_set_max_length = (function(self,
                                                      self_2, val)
                                                      {
                                                        self["maxLength"] = val;
                                                      });
var h$webkit_dom_html_text_area_element_get_max_length;
h$webkit_dom_html_text_area_element_get_max_length = (function(self,
                                                      self_2)
                                                      {
                                                        return self["maxLength"];
                                                      });
var h$webkit_dom_html_text_area_element_set_name;
h$webkit_dom_html_text_area_element_set_name = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["name"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_text_area_element_get_name;
h$webkit_dom_html_text_area_element_get_name = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["name"]);
                                                });
var h$webkit_dom_html_text_area_element_set_placeholder;
h$webkit_dom_html_text_area_element_set_placeholder = (function(self,
                                                       self_2, val, val_2)
                                                       {
                                                         self["placeholder"] = h$decodeUtf8z(val,
                                                         val_2);
                                                       });
var h$webkit_dom_html_text_area_element_get_placeholder;
h$webkit_dom_html_text_area_element_get_placeholder = (function(self,
                                                       self_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return h$encodeUtf8(self["placeholder"]);
                                                       });
var h$webkit_dom_html_text_area_element_set_read_only;
h$webkit_dom_html_text_area_element_set_read_only = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["readOnly"] = val;
                                                     });
var h$webkit_dom_html_text_area_element_get_read_only;
h$webkit_dom_html_text_area_element_get_read_only = (function(self,
                                                     self_2)
                                                     {
                                                       return self["readOnly"];
                                                     });
var h$webkit_dom_html_text_area_element_set_required;
h$webkit_dom_html_text_area_element_set_required = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["required"] = val;
                                                    });
var h$webkit_dom_html_text_area_element_get_required;
h$webkit_dom_html_text_area_element_get_required = (function(self,
                                                    self_2)
                                                    {
                                                      return self["required"];
                                                    });
var h$webkit_dom_html_text_area_element_set_rows;
h$webkit_dom_html_text_area_element_set_rows = (function(self,
                                                self_2, val)
                                                {
                                                  self["rows"] = val;
                                                });
var h$webkit_dom_html_text_area_element_get_rows;
h$webkit_dom_html_text_area_element_get_rows = (function(self,
                                                self_2)
                                                {
                                                  return self["rows"];
                                                });
var h$webkit_dom_html_text_area_element_set_wrap;
h$webkit_dom_html_text_area_element_set_wrap = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["wrap"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_text_area_element_get_wrap;
h$webkit_dom_html_text_area_element_get_wrap = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["wrap"]);
                                                });
var h$webkit_dom_html_text_area_element_set_default_value;
h$webkit_dom_html_text_area_element_set_default_value = (function(self,
                                                         self_2, val, val_2)
                                                         {
                                                           self["defaultValue"] = h$decodeUtf8z(val,
                                                           val_2);
                                                         });
var h$webkit_dom_html_text_area_element_get_default_value;
h$webkit_dom_html_text_area_element_get_default_value = (function(self,
                                                         self_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return h$encodeUtf8(self["defaultValue"]);
                                                         });
var h$webkit_dom_html_text_area_element_set_value;
h$webkit_dom_html_text_area_element_set_value = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["value"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_text_area_element_get_value;
h$webkit_dom_html_text_area_element_get_value = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["value"]);
                                                 });
var h$webkit_dom_html_text_area_element_get_text_length;
h$webkit_dom_html_text_area_element_get_text_length = (function(self,
                                                       self_2)
                                                       {
                                                         return self["textLength"];
                                                       });
var h$webkit_dom_html_text_area_element_get_will_validate;
h$webkit_dom_html_text_area_element_get_will_validate = (function(self,
                                                         self_2)
                                                         {
                                                           return self["willValidate"];
                                                         });
var h$webkit_dom_html_text_area_element_get_validity;
h$webkit_dom_html_text_area_element_get_validity = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["validity"];
                                                    });
var h$webkit_dom_html_text_area_element_get_validation_message;
h$webkit_dom_html_text_area_element_get_validation_message = (function(self,
                                                              self_2)
                                                              {
                                                                h$ret1 = 0;
                                                                return h$encodeUtf8(self["validationMessage"]);
                                                              });
var h$webkit_dom_html_text_area_element_get_labels;
h$webkit_dom_html_text_area_element_get_labels = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["labels"];
                                                  });
var h$webkit_dom_html_text_area_element_set_selection_start;
h$webkit_dom_html_text_area_element_set_selection_start = (function(self,
                                                           self_2, val)
                                                           {
                                                             self["selectionStart"] = val;
                                                           });
var h$webkit_dom_html_text_area_element_get_selection_start;
h$webkit_dom_html_text_area_element_get_selection_start = (function(self,
                                                           self_2)
                                                           {
                                                             return self["selectionStart"];
                                                           });
var h$webkit_dom_html_text_area_element_set_selection_end;
h$webkit_dom_html_text_area_element_set_selection_end = (function(self,
                                                         self_2, val)
                                                         {
                                                           self["selectionEnd"] = val;
                                                         });
var h$webkit_dom_html_text_area_element_get_selection_end;
h$webkit_dom_html_text_area_element_get_selection_end = (function(self,
                                                         self_2)
                                                         {
                                                           return self["selectionEnd"];
                                                         });
var h$webkit_dom_html_text_area_element_set_selection_direction;
h$webkit_dom_html_text_area_element_set_selection_direction = (function(self,
                                                               self_2, val,
                                                               val_2)
                                                               {
                                                                 self["selectionDirection"] = h$decodeUtf8z(val,
                                                                 val_2);
                                                               });
var h$webkit_dom_html_text_area_element_get_selection_direction;
h$webkit_dom_html_text_area_element_get_selection_direction = (function(self,
                                                               self_2)
                                                               {
                                                                 h$ret1 = 0;
                                                                 return h$encodeUtf8(self["selectionDirection"]);
                                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_table_section_element_get_type = (function()
                                                    {
                                                      return h$g_get_type(HTMLTableSectionElement);
                                                    });
var h$webkit_dom_html_table_section_element_insert_row;
h$webkit_dom_html_table_section_element_insert_row = (function(self,
                                                      self_2, index)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["insertRow"](index);
                                                      });
var h$webkit_dom_html_table_section_element_delete_row;
h$webkit_dom_html_table_section_element_delete_row = (function(self,
                                                      self_2, index)
                                                      {
                                                        return self["deleteRow"](index);
                                                      });
var h$webkit_dom_html_table_section_element_set_align;
h$webkit_dom_html_table_section_element_set_align = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["align"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_table_section_element_get_align;
h$webkit_dom_html_table_section_element_get_align = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["align"]);
                                                     });
var h$webkit_dom_html_table_section_element_set_ch;
h$webkit_dom_html_table_section_element_set_ch = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["ch"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_table_section_element_get_ch;
h$webkit_dom_html_table_section_element_get_ch = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["ch"]);
                                                  });
var h$webkit_dom_html_table_section_element_set_ch_off;
h$webkit_dom_html_table_section_element_set_ch_off = (function(self,
                                                      self_2, val, val_2)
                                                      {
                                                        self["chOff"] = h$decodeUtf8z(val,
                                                        val_2);
                                                      });
var h$webkit_dom_html_table_section_element_get_ch_off;
h$webkit_dom_html_table_section_element_get_ch_off = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return h$encodeUtf8(self["chOff"]);
                                                      });
var h$webkit_dom_html_table_section_element_set_v_align;
h$webkit_dom_html_table_section_element_set_v_align = (function(self,
                                                       self_2, val, val_2)
                                                       {
                                                         self["vAlign"] = h$decodeUtf8z(val,
                                                         val_2);
                                                       });
var h$webkit_dom_html_table_section_element_get_v_align;
h$webkit_dom_html_table_section_element_get_v_align = (function(self,
                                                       self_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return h$encodeUtf8(self["vAlign"]);
                                                       });
var h$webkit_dom_html_table_section_element_get_rows;
h$webkit_dom_html_table_section_element_get_rows = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["rows"];
                                                    });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_table_row_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLTableRowElement);
                                                });
var h$webkit_dom_html_table_row_element_insert_cell;
h$webkit_dom_html_table_row_element_insert_cell = (function(self,
                                                   self_2, index)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["insertCell"](index);
                                                   });
var h$webkit_dom_html_table_row_element_delete_cell;
h$webkit_dom_html_table_row_element_delete_cell = (function(self,
                                                   self_2, index)
                                                   {
                                                     return self["deleteCell"](index);
                                                   });
var h$webkit_dom_html_table_row_element_get_row_index;
h$webkit_dom_html_table_row_element_get_row_index = (function(self,
                                                     self_2)
                                                     {
                                                       return self["rowIndex"];
                                                     });
var h$webkit_dom_html_table_row_element_get_section_row_index;
h$webkit_dom_html_table_row_element_get_section_row_index = (function(self,
                                                             self_2)
                                                             {
                                                               return self["sectionRowIndex"];
                                                             });
var h$webkit_dom_html_table_row_element_get_cells;
h$webkit_dom_html_table_row_element_get_cells = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["cells"];
                                                 });
var h$webkit_dom_html_table_row_element_set_align;
h$webkit_dom_html_table_row_element_set_align = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["align"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_table_row_element_get_align;
h$webkit_dom_html_table_row_element_get_align = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["align"]);
                                                 });
var h$webkit_dom_html_table_row_element_set_bg_color;
h$webkit_dom_html_table_row_element_set_bg_color = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["bgColor"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_table_row_element_get_bg_color;
h$webkit_dom_html_table_row_element_get_bg_color = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["bgColor"]);
                                                    });
var h$webkit_dom_html_table_row_element_set_ch;
h$webkit_dom_html_table_row_element_set_ch = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["ch"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_table_row_element_get_ch;
h$webkit_dom_html_table_row_element_get_ch = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["ch"]);
                                              });
var h$webkit_dom_html_table_row_element_set_ch_off;
h$webkit_dom_html_table_row_element_set_ch_off = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["chOff"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_table_row_element_get_ch_off;
h$webkit_dom_html_table_row_element_get_ch_off = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["chOff"]);
                                                  });
var h$webkit_dom_html_table_row_element_set_v_align;
h$webkit_dom_html_table_row_element_set_v_align = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["vAlign"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_table_row_element_get_v_align;
h$webkit_dom_html_table_row_element_get_v_align = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["vAlign"]);
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_table_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLTableElement);
                                            });
var h$webkit_dom_html_table_element_create_t_head;
h$webkit_dom_html_table_element_create_t_head = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["createTHead"]();
                                                 });
var h$webkit_dom_html_table_element_delete_t_head;
h$webkit_dom_html_table_element_delete_t_head = (function(self,
                                                 self_2)
                                                 {
                                                   return self["deleteTHead"]();
                                                 });
var h$webkit_dom_html_table_element_create_t_foot;
h$webkit_dom_html_table_element_create_t_foot = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["createTFoot"]();
                                                 });
var h$webkit_dom_html_table_element_delete_t_foot;
h$webkit_dom_html_table_element_delete_t_foot = (function(self,
                                                 self_2)
                                                 {
                                                   return self["deleteTFoot"]();
                                                 });
var h$webkit_dom_html_table_element_create_t_body;
h$webkit_dom_html_table_element_create_t_body = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["createTBody"]();
                                                 });
var h$webkit_dom_html_table_element_create_caption;
h$webkit_dom_html_table_element_create_caption = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["createCaption"]();
                                                  });
var h$webkit_dom_html_table_element_delete_caption;
h$webkit_dom_html_table_element_delete_caption = (function(self,
                                                  self_2)
                                                  {
                                                    return self["deleteCaption"]();
                                                  });
var h$webkit_dom_html_table_element_insert_row;
h$webkit_dom_html_table_element_insert_row = (function(self,
                                              self_2, index)
                                              {
                                                h$ret1 = 0;
                                                return self["insertRow"](index);
                                              });
var h$webkit_dom_html_table_element_delete_row;
h$webkit_dom_html_table_element_delete_row = (function(self,
                                              self_2, index)
                                              {
                                                return self["deleteRow"](index);
                                              });
var h$webkit_dom_html_table_element_set_caption;
h$webkit_dom_html_table_element_set_caption = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["caption"] = val;
                                               });
var h$webkit_dom_html_table_element_get_caption;
h$webkit_dom_html_table_element_get_caption = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["caption"];
                                               });
var h$webkit_dom_html_table_element_set_t_head;
h$webkit_dom_html_table_element_set_t_head = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["tHead"] = val;
                                              });
var h$webkit_dom_html_table_element_get_t_head;
h$webkit_dom_html_table_element_get_t_head = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["tHead"];
                                              });
var h$webkit_dom_html_table_element_set_t_foot;
h$webkit_dom_html_table_element_set_t_foot = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["tFoot"] = val;
                                              });
var h$webkit_dom_html_table_element_get_t_foot;
h$webkit_dom_html_table_element_get_t_foot = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["tFoot"];
                                              });
var h$webkit_dom_html_table_element_get_rows;
h$webkit_dom_html_table_element_get_rows = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["rows"];
                                            });
var h$webkit_dom_html_table_element_get_t_bodies;
h$webkit_dom_html_table_element_get_t_bodies = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["tBodies"];
                                                });
var h$webkit_dom_html_table_element_set_align;
h$webkit_dom_html_table_element_set_align = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["align"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_table_element_get_align;
h$webkit_dom_html_table_element_get_align = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["align"]);
                                             });
var h$webkit_dom_html_table_element_set_bg_color;
h$webkit_dom_html_table_element_set_bg_color = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["bgColor"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_table_element_get_bg_color;
h$webkit_dom_html_table_element_get_bg_color = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["bgColor"]);
                                                });
var h$webkit_dom_html_table_element_set_border;
h$webkit_dom_html_table_element_set_border = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["border"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_table_element_get_border;
h$webkit_dom_html_table_element_get_border = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["border"]);
                                              });
var h$webkit_dom_html_table_element_set_cell_padding;
h$webkit_dom_html_table_element_set_cell_padding = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["cellPadding"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_table_element_get_cell_padding;
h$webkit_dom_html_table_element_get_cell_padding = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["cellPadding"]);
                                                    });
var h$webkit_dom_html_table_element_set_cell_spacing;
h$webkit_dom_html_table_element_set_cell_spacing = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["cellSpacing"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_table_element_get_cell_spacing;
h$webkit_dom_html_table_element_get_cell_spacing = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["cellSpacing"]);
                                                    });
var h$webkit_dom_html_table_element_set_frame;
h$webkit_dom_html_table_element_set_frame = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["frame"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_table_element_get_frame;
h$webkit_dom_html_table_element_get_frame = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["frame"]);
                                             });
var h$webkit_dom_html_table_element_set_rules;
h$webkit_dom_html_table_element_set_rules = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["rules"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_table_element_get_rules;
h$webkit_dom_html_table_element_get_rules = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["rules"]);
                                             });
var h$webkit_dom_html_table_element_set_summary;
h$webkit_dom_html_table_element_set_summary = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["summary"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_table_element_get_summary;
h$webkit_dom_html_table_element_get_summary = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["summary"]);
                                               });
var h$webkit_dom_html_table_element_set_width;
h$webkit_dom_html_table_element_set_width = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["width"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_table_element_get_width;
h$webkit_dom_html_table_element_get_width = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["width"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_table_col_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLTableColElement);
                                                });
var h$webkit_dom_html_table_col_element_set_align;
h$webkit_dom_html_table_col_element_set_align = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["align"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_table_col_element_get_align;
h$webkit_dom_html_table_col_element_get_align = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["align"]);
                                                 });
var h$webkit_dom_html_table_col_element_set_ch;
h$webkit_dom_html_table_col_element_set_ch = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["ch"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_table_col_element_get_ch;
h$webkit_dom_html_table_col_element_get_ch = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["ch"]);
                                              });
var h$webkit_dom_html_table_col_element_set_ch_off;
h$webkit_dom_html_table_col_element_set_ch_off = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["chOff"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_table_col_element_get_ch_off;
h$webkit_dom_html_table_col_element_get_ch_off = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["chOff"]);
                                                  });
var h$webkit_dom_html_table_col_element_set_span;
h$webkit_dom_html_table_col_element_set_span = (function(self,
                                                self_2, val)
                                                {
                                                  self["span"] = val;
                                                });
var h$webkit_dom_html_table_col_element_get_span;
h$webkit_dom_html_table_col_element_get_span = (function(self,
                                                self_2)
                                                {
                                                  return self["span"];
                                                });
var h$webkit_dom_html_table_col_element_set_v_align;
h$webkit_dom_html_table_col_element_set_v_align = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["vAlign"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_table_col_element_get_v_align;
h$webkit_dom_html_table_col_element_get_v_align = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["vAlign"]);
                                                   });
var h$webkit_dom_html_table_col_element_set_width;
h$webkit_dom_html_table_col_element_set_width = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["width"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_table_col_element_get_width;
h$webkit_dom_html_table_col_element_get_width = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["width"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_table_cell_element_get_type = (function()
                                                 {
                                                   return h$g_get_type(HTMLTableCellElement);
                                                 });
var h$webkit_dom_html_table_cell_element_get_cell_index;
h$webkit_dom_html_table_cell_element_get_cell_index = (function(self,
                                                       self_2)
                                                       {
                                                         return self["cellIndex"];
                                                       });
var h$webkit_dom_html_table_cell_element_set_abbr;
h$webkit_dom_html_table_cell_element_set_abbr = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["abbr"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_table_cell_element_get_abbr;
h$webkit_dom_html_table_cell_element_get_abbr = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["abbr"]);
                                                 });
var h$webkit_dom_html_table_cell_element_set_align;
h$webkit_dom_html_table_cell_element_set_align = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["align"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_table_cell_element_get_align;
h$webkit_dom_html_table_cell_element_get_align = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["align"]);
                                                  });
var h$webkit_dom_html_table_cell_element_set_axis;
h$webkit_dom_html_table_cell_element_set_axis = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["axis"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_table_cell_element_get_axis;
h$webkit_dom_html_table_cell_element_get_axis = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["axis"]);
                                                 });
var h$webkit_dom_html_table_cell_element_set_bg_color;
h$webkit_dom_html_table_cell_element_set_bg_color = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["bgColor"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_table_cell_element_get_bg_color;
h$webkit_dom_html_table_cell_element_get_bg_color = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["bgColor"]);
                                                     });
var h$webkit_dom_html_table_cell_element_set_ch;
h$webkit_dom_html_table_cell_element_set_ch = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["ch"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_table_cell_element_get_ch;
h$webkit_dom_html_table_cell_element_get_ch = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["ch"]);
                                               });
var h$webkit_dom_html_table_cell_element_set_ch_off;
h$webkit_dom_html_table_cell_element_set_ch_off = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["chOff"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_table_cell_element_get_ch_off;
h$webkit_dom_html_table_cell_element_get_ch_off = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["chOff"]);
                                                   });
var h$webkit_dom_html_table_cell_element_set_col_span;
h$webkit_dom_html_table_cell_element_set_col_span = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["colSpan"] = val;
                                                     });
var h$webkit_dom_html_table_cell_element_get_col_span;
h$webkit_dom_html_table_cell_element_get_col_span = (function(self,
                                                     self_2)
                                                     {
                                                       return self["colSpan"];
                                                     });
var h$webkit_dom_html_table_cell_element_set_headers;
h$webkit_dom_html_table_cell_element_set_headers = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["headers"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_table_cell_element_get_headers;
h$webkit_dom_html_table_cell_element_get_headers = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["headers"]);
                                                    });
var h$webkit_dom_html_table_cell_element_set_height;
h$webkit_dom_html_table_cell_element_set_height = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["height"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_table_cell_element_get_height;
h$webkit_dom_html_table_cell_element_get_height = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["height"]);
                                                   });
var h$webkit_dom_html_table_cell_element_set_no_wrap;
h$webkit_dom_html_table_cell_element_set_no_wrap = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["noWrap"] = val;
                                                    });
var h$webkit_dom_html_table_cell_element_get_no_wrap;
h$webkit_dom_html_table_cell_element_get_no_wrap = (function(self,
                                                    self_2)
                                                    {
                                                      return self["noWrap"];
                                                    });
var h$webkit_dom_html_table_cell_element_set_row_span;
h$webkit_dom_html_table_cell_element_set_row_span = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["rowSpan"] = val;
                                                     });
var h$webkit_dom_html_table_cell_element_get_row_span;
h$webkit_dom_html_table_cell_element_get_row_span = (function(self,
                                                     self_2)
                                                     {
                                                       return self["rowSpan"];
                                                     });
var h$webkit_dom_html_table_cell_element_set_scope;
h$webkit_dom_html_table_cell_element_set_scope = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["scope"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_table_cell_element_get_scope;
h$webkit_dom_html_table_cell_element_get_scope = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["scope"]);
                                                  });
var h$webkit_dom_html_table_cell_element_set_v_align;
h$webkit_dom_html_table_cell_element_set_v_align = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["vAlign"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_table_cell_element_get_v_align;
h$webkit_dom_html_table_cell_element_get_v_align = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["vAlign"]);
                                                    });
var h$webkit_dom_html_table_cell_element_set_width;
h$webkit_dom_html_table_cell_element_set_width = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["width"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_table_cell_element_get_width;
h$webkit_dom_html_table_cell_element_get_width = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["width"]);
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_table_caption_element_get_type = (function()
                                                    {
                                                      return h$g_get_type(HTMLTableCaptionElement);
                                                    });
var h$webkit_dom_html_table_caption_element_set_align;
h$webkit_dom_html_table_caption_element_set_align = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["align"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_table_caption_element_get_align;
h$webkit_dom_html_table_caption_element_get_align = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["align"]);
                                                     });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_style_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLStyleElement);
                                            });
var h$webkit_dom_html_style_element_set_disabled;
h$webkit_dom_html_style_element_set_disabled = (function(self,
                                                self_2, val)
                                                {
                                                  self["disabled"] = val;
                                                });
var h$webkit_dom_html_style_element_get_disabled;
h$webkit_dom_html_style_element_get_disabled = (function(self,
                                                self_2)
                                                {
                                                  return self["disabled"];
                                                });
var h$webkit_dom_html_style_element_set_scoped;
h$webkit_dom_html_style_element_set_scoped = (function(self,
                                              self_2, val)
                                              {
                                                self["scoped"] = val;
                                              });
var h$webkit_dom_html_style_element_get_scoped;
h$webkit_dom_html_style_element_get_scoped = (function(self,
                                              self_2)
                                              {
                                                return self["scoped"];
                                              });
var h$webkit_dom_html_style_element_set_media;
h$webkit_dom_html_style_element_set_media = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["media"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_style_element_get_media;
h$webkit_dom_html_style_element_get_media = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["media"]);
                                             });
var h$webkit_dom_html_style_element_get_sheet;
h$webkit_dom_html_style_element_get_sheet = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["sheet"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_select_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLSelectElement);
                                             });
var h$webkit_dom_html_select_element_item;
h$webkit_dom_html_select_element_item = (function(self,
                                         self_2, index)
                                         {
                                           h$ret1 = 0;
                                           return self["item"](index);
                                         });
var h$webkit_dom_html_select_element_named_item;
h$webkit_dom_html_select_element_named_item = (function(self,
                                               self_2, name, name_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["namedItem"](h$decodeUtf8z(name,
                                                 name_2));
                                               });
var h$webkit_dom_html_select_element_add;
h$webkit_dom_html_select_element_add = (function(self,
                                        self_2, element, element_2,
                                        before, before_2)
                                        {
                                          return self["add"](element,
                                          before);
                                        });
var h$webkit_dom_html_select_element_remove;
h$webkit_dom_html_select_element_remove = (function(self,
                                           self_2, index)
                                           {
                                             return self["remove"](index);
                                           });
var h$webkit_dom_html_select_element_check_validity;
h$webkit_dom_html_select_element_check_validity = (function(self,
                                                   self_2)
                                                   {
                                                     return self["checkValidity"]();
                                                   });
var h$webkit_dom_html_select_element_set_custom_validity;
h$webkit_dom_html_select_element_set_custom_validity = (function(self,
                                                        self_2, error, error_2)
                                                        {
                                                          return self["setCustomValidity"](h$decodeUtf8z(error,
                                                          error_2));
                                                        });
var h$webkit_dom_html_select_element_set_autofocus;
h$webkit_dom_html_select_element_set_autofocus = (function(self,
                                                  self_2, val)
                                                  {
                                                    self["autofocus"] = val;
                                                  });
var h$webkit_dom_html_select_element_get_autofocus;
h$webkit_dom_html_select_element_get_autofocus = (function(self,
                                                  self_2)
                                                  {
                                                    return self["autofocus"];
                                                  });
var h$webkit_dom_html_select_element_set_disabled;
h$webkit_dom_html_select_element_set_disabled = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["disabled"] = val;
                                                 });
var h$webkit_dom_html_select_element_get_disabled;
h$webkit_dom_html_select_element_get_disabled = (function(self,
                                                 self_2)
                                                 {
                                                   return self["disabled"];
                                                 });
var h$webkit_dom_html_select_element_get_form;
h$webkit_dom_html_select_element_get_form = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["form"];
                                             });
var h$webkit_dom_html_select_element_set_multiple;
h$webkit_dom_html_select_element_set_multiple = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["multiple"] = val;
                                                 });
var h$webkit_dom_html_select_element_get_multiple;
h$webkit_dom_html_select_element_get_multiple = (function(self,
                                                 self_2)
                                                 {
                                                   return self["multiple"];
                                                 });
var h$webkit_dom_html_select_element_set_name;
h$webkit_dom_html_select_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_select_element_get_name;
h$webkit_dom_html_select_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_select_element_set_required;
h$webkit_dom_html_select_element_set_required = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["required"] = val;
                                                 });
var h$webkit_dom_html_select_element_get_required;
h$webkit_dom_html_select_element_get_required = (function(self,
                                                 self_2)
                                                 {
                                                   return self["required"];
                                                 });
var h$webkit_dom_html_select_element_set_size;
h$webkit_dom_html_select_element_set_size = (function(self,
                                             self_2, val)
                                             {
                                               self["size"] = val;
                                             });
var h$webkit_dom_html_select_element_get_size;
h$webkit_dom_html_select_element_get_size = (function(self,
                                             self_2)
                                             {
                                               return self["size"];
                                             });
var h$webkit_dom_html_select_element_get_options;
h$webkit_dom_html_select_element_get_options = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["options"];
                                                });
var h$webkit_dom_html_select_element_set_length;
h$webkit_dom_html_select_element_set_length = (function(self,
                                               self_2, val)
                                               {
                                                 self["length"] = val;
                                               });
var h$webkit_dom_html_select_element_get_length;
h$webkit_dom_html_select_element_get_length = (function(self,
                                               self_2)
                                               {
                                                 return self["length"];
                                               });
var h$webkit_dom_html_select_element_get_selected_options;
h$webkit_dom_html_select_element_get_selected_options = (function(self,
                                                         self_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return self["selectedOptions"];
                                                         });
var h$webkit_dom_html_select_element_set_selected_index;
h$webkit_dom_html_select_element_set_selected_index = (function(self,
                                                       self_2, val)
                                                       {
                                                         self["selectedIndex"] = val;
                                                       });
var h$webkit_dom_html_select_element_get_selected_index;
h$webkit_dom_html_select_element_get_selected_index = (function(self,
                                                       self_2)
                                                       {
                                                         return self["selectedIndex"];
                                                       });
var h$webkit_dom_html_select_element_set_value;
h$webkit_dom_html_select_element_set_value = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["value"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_select_element_get_value;
h$webkit_dom_html_select_element_get_value = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["value"]);
                                              });
var h$webkit_dom_html_select_element_get_will_validate;
h$webkit_dom_html_select_element_get_will_validate = (function(self,
                                                      self_2)
                                                      {
                                                        return self["willValidate"];
                                                      });
var h$webkit_dom_html_select_element_get_validity;
h$webkit_dom_html_select_element_get_validity = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["validity"];
                                                 });
var h$webkit_dom_html_select_element_get_validation_message;
h$webkit_dom_html_select_element_get_validation_message = (function(self,
                                                           self_2)
                                                           {
                                                             h$ret1 = 0;
                                                             return h$encodeUtf8(self["validationMessage"]);
                                                           });
var h$webkit_dom_html_select_element_get_labels;
h$webkit_dom_html_select_element_get_labels = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["labels"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_script_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLScriptElement);
                                             });
var h$webkit_dom_html_script_element_set_text;
h$webkit_dom_html_script_element_set_text = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["text"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_script_element_get_text;
h$webkit_dom_html_script_element_get_text = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["text"]);
                                             });
var h$webkit_dom_html_script_element_set_html_for;
h$webkit_dom_html_script_element_set_html_for = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["htmlFor"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_script_element_get_html_for;
h$webkit_dom_html_script_element_get_html_for = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["htmlFor"]);
                                                 });
var h$webkit_dom_html_script_element_set_event;
h$webkit_dom_html_script_element_set_event = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["event"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_script_element_get_event;
h$webkit_dom_html_script_element_get_event = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["event"]);
                                              });
var h$webkit_dom_html_script_element_set_charset;
h$webkit_dom_html_script_element_set_charset = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["charset"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_script_element_get_charset;
h$webkit_dom_html_script_element_get_charset = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["charset"]);
                                                });
var h$webkit_dom_html_script_element_set_async;
h$webkit_dom_html_script_element_set_async = (function(self,
                                              self_2, val)
                                              {
                                                self["async"] = val;
                                              });
var h$webkit_dom_html_script_element_get_async;
h$webkit_dom_html_script_element_get_async = (function(self,
                                              self_2)
                                              {
                                                return self["async"];
                                              });
var h$webkit_dom_html_script_element_set_defer;
h$webkit_dom_html_script_element_set_defer = (function(self,
                                              self_2, val)
                                              {
                                                self["defer"] = val;
                                              });
var h$webkit_dom_html_script_element_get_defer;
h$webkit_dom_html_script_element_get_defer = (function(self,
                                              self_2)
                                              {
                                                return self["defer"];
                                              });
var h$webkit_dom_html_script_element_set_src;
h$webkit_dom_html_script_element_set_src = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["src"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_script_element_get_src;
h$webkit_dom_html_script_element_get_src = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["src"]);
                                            });
var h$webkit_dom_html_script_element_set_cross_origin;
h$webkit_dom_html_script_element_set_cross_origin = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["crossOrigin"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_script_element_get_cross_origin;
h$webkit_dom_html_script_element_get_cross_origin = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["crossOrigin"]);
                                                     });
var h$webkit_dom_html_script_element_set_nonce;
h$webkit_dom_html_script_element_set_nonce = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["nonce"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_script_element_get_nonce;
h$webkit_dom_html_script_element_get_nonce = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["nonce"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_quote_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLQuoteElement);
                                            });
var h$webkit_dom_html_quote_element_set_cite;
h$webkit_dom_html_quote_element_set_cite = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["cite"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_quote_element_get_cite;
h$webkit_dom_html_quote_element_get_cite = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["cite"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_pre_element_get_type = (function()
                                          {
                                            return h$g_get_type(HTMLPreElement);
                                          });
var h$webkit_dom_html_pre_element_set_width;
h$webkit_dom_html_pre_element_set_width = (function(self,
                                           self_2, val)
                                           {
                                             self["width"] = val;
                                           });
var h$webkit_dom_html_pre_element_get_width;
h$webkit_dom_html_pre_element_get_width = (function(self,
                                           self_2)
                                           {
                                             return self["width"];
                                           });
var h$webkit_dom_html_pre_element_set_wrap;
h$webkit_dom_html_pre_element_set_wrap = (function(self,
                                          self_2, val)
                                          {
                                            self["wrap"] = val;
                                          });
var h$webkit_dom_html_pre_element_get_wrap;
h$webkit_dom_html_pre_element_get_wrap = (function(self,
                                          self_2)
                                          {
                                            return self["wrap"];
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_param_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLParamElement);
                                            });
var h$webkit_dom_html_param_element_set_name;
h$webkit_dom_html_param_element_set_name = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["name"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_param_element_get_name;
h$webkit_dom_html_param_element_get_name = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["name"]);
                                            });
var h$webkit_dom_html_param_element_set_value;
h$webkit_dom_html_param_element_set_value = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["value"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_param_element_get_value;
h$webkit_dom_html_param_element_get_value = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["value"]);
                                             });
var h$webkit_dom_html_param_element_set_value_type;
h$webkit_dom_html_param_element_set_value_type = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["valueType"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_param_element_get_value_type;
h$webkit_dom_html_param_element_get_value_type = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["valueType"]);
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_paragraph_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLParagraphElement);
                                                });
var h$webkit_dom_html_paragraph_element_set_align;
h$webkit_dom_html_paragraph_element_set_align = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["align"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_paragraph_element_get_align;
h$webkit_dom_html_paragraph_element_get_align = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["align"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_options_collection_get_type = (function()
                                                 {
                                                   return h$g_get_type(HTMLOptionsCollection);
                                                 });
var h$webkit_dom_html_options_collection_set_selected_index;
h$webkit_dom_html_options_collection_set_selected_index = (function(self,
                                                           self_2, val)
                                                           {
                                                             self["selectedIndex"] = val;
                                                           });
var h$webkit_dom_html_options_collection_get_selected_index;
h$webkit_dom_html_options_collection_get_selected_index = (function(self,
                                                           self_2)
                                                           {
                                                             return self["selectedIndex"];
                                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_option_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLOptionElement);
                                             });
var h$webkit_dom_html_option_element_set_disabled;
h$webkit_dom_html_option_element_set_disabled = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["disabled"] = val;
                                                 });
var h$webkit_dom_html_option_element_get_disabled;
h$webkit_dom_html_option_element_get_disabled = (function(self,
                                                 self_2)
                                                 {
                                                   return self["disabled"];
                                                 });
var h$webkit_dom_html_option_element_get_form;
h$webkit_dom_html_option_element_get_form = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["form"];
                                             });
var h$webkit_dom_html_option_element_set_label;
h$webkit_dom_html_option_element_set_label = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["label"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_option_element_get_label;
h$webkit_dom_html_option_element_get_label = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["label"]);
                                              });
var h$webkit_dom_html_option_element_set_default_selected;
h$webkit_dom_html_option_element_set_default_selected = (function(self,
                                                         self_2, val)
                                                         {
                                                           self["defaultSelected"] = val;
                                                         });
var h$webkit_dom_html_option_element_get_default_selected;
h$webkit_dom_html_option_element_get_default_selected = (function(self,
                                                         self_2)
                                                         {
                                                           return self["defaultSelected"];
                                                         });
var h$webkit_dom_html_option_element_set_selected;
h$webkit_dom_html_option_element_set_selected = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["selected"] = val;
                                                 });
var h$webkit_dom_html_option_element_get_selected;
h$webkit_dom_html_option_element_get_selected = (function(self,
                                                 self_2)
                                                 {
                                                   return self["selected"];
                                                 });
var h$webkit_dom_html_option_element_set_value;
h$webkit_dom_html_option_element_set_value = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["value"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_option_element_get_value;
h$webkit_dom_html_option_element_get_value = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["value"]);
                                              });
var h$webkit_dom_html_option_element_get_text;
h$webkit_dom_html_option_element_get_text = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["text"]);
                                             });
var h$webkit_dom_html_option_element_get_index;
h$webkit_dom_html_option_element_get_index = (function(self,
                                              self_2)
                                              {
                                                return self["index"];
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_opt_group_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLOptGroupElement);
                                                });
var h$webkit_dom_html_opt_group_element_set_disabled;
h$webkit_dom_html_opt_group_element_set_disabled = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["disabled"] = val;
                                                    });
var h$webkit_dom_html_opt_group_element_get_disabled;
h$webkit_dom_html_opt_group_element_get_disabled = (function(self,
                                                    self_2)
                                                    {
                                                      return self["disabled"];
                                                    });
var h$webkit_dom_html_opt_group_element_set_label;
h$webkit_dom_html_opt_group_element_set_label = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["label"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_opt_group_element_get_label;
h$webkit_dom_html_opt_group_element_get_label = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["label"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_htmlo_list_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLOListElement);
                                            });
var h$webkit_dom_htmlo_list_element_set_compact;
h$webkit_dom_htmlo_list_element_set_compact = (function(self,
                                               self_2, val)
                                               {
                                                 self["compact"] = val;
                                               });
var h$webkit_dom_htmlo_list_element_get_compact;
h$webkit_dom_htmlo_list_element_get_compact = (function(self,
                                               self_2)
                                               {
                                                 return self["compact"];
                                               });
var h$webkit_dom_htmlo_list_element_set_start;
h$webkit_dom_htmlo_list_element_set_start = (function(self,
                                             self_2, val)
                                             {
                                               self["start"] = val;
                                             });
var h$webkit_dom_htmlo_list_element_get_start;
h$webkit_dom_htmlo_list_element_get_start = (function(self,
                                             self_2)
                                             {
                                               return self["start"];
                                             });
var h$webkit_dom_htmlo_list_element_set_reversed;
h$webkit_dom_htmlo_list_element_set_reversed = (function(self,
                                                self_2, val)
                                                {
                                                  self["reversed"] = val;
                                                });
var h$webkit_dom_htmlo_list_element_get_reversed;
h$webkit_dom_htmlo_list_element_get_reversed = (function(self,
                                                self_2)
                                                {
                                                  return self["reversed"];
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_object_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLObjectElement);
                                             });
var h$webkit_dom_html_object_element_check_validity;
h$webkit_dom_html_object_element_check_validity = (function(self,
                                                   self_2)
                                                   {
                                                     return self["checkValidity"]();
                                                   });
var h$webkit_dom_html_object_element_set_custom_validity;
h$webkit_dom_html_object_element_set_custom_validity = (function(self,
                                                        self_2, error, error_2)
                                                        {
                                                          return self["setCustomValidity"](h$decodeUtf8z(error,
                                                          error_2));
                                                        });
var h$webkit_dom_html_object_element_get_form;
h$webkit_dom_html_object_element_get_form = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["form"];
                                             });
var h$webkit_dom_html_object_element_set_code;
h$webkit_dom_html_object_element_set_code = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["code"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_object_element_get_code;
h$webkit_dom_html_object_element_get_code = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["code"]);
                                             });
var h$webkit_dom_html_object_element_set_align;
h$webkit_dom_html_object_element_set_align = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["align"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_object_element_get_align;
h$webkit_dom_html_object_element_get_align = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["align"]);
                                              });
var h$webkit_dom_html_object_element_set_archive;
h$webkit_dom_html_object_element_set_archive = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["archive"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_object_element_get_archive;
h$webkit_dom_html_object_element_get_archive = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["archive"]);
                                                });
var h$webkit_dom_html_object_element_set_border;
h$webkit_dom_html_object_element_set_border = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["border"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_object_element_get_border;
h$webkit_dom_html_object_element_get_border = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["border"]);
                                               });
var h$webkit_dom_html_object_element_set_code_base;
h$webkit_dom_html_object_element_set_code_base = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["codeBase"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_object_element_get_code_base;
h$webkit_dom_html_object_element_get_code_base = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["codeBase"]);
                                                  });
var h$webkit_dom_html_object_element_set_code_type;
h$webkit_dom_html_object_element_set_code_type = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["codeType"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_object_element_get_code_type;
h$webkit_dom_html_object_element_get_code_type = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["codeType"]);
                                                  });
var h$webkit_dom_html_object_element_set_data;
h$webkit_dom_html_object_element_set_data = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["data"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_object_element_get_data;
h$webkit_dom_html_object_element_get_data = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["data"]);
                                             });
var h$webkit_dom_html_object_element_set_declare;
h$webkit_dom_html_object_element_set_declare = (function(self,
                                                self_2, val)
                                                {
                                                  self["declare"] = val;
                                                });
var h$webkit_dom_html_object_element_get_declare;
h$webkit_dom_html_object_element_get_declare = (function(self,
                                                self_2)
                                                {
                                                  return self["declare"];
                                                });
var h$webkit_dom_html_object_element_set_height;
h$webkit_dom_html_object_element_set_height = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["height"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_object_element_get_height;
h$webkit_dom_html_object_element_get_height = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["height"]);
                                               });
var h$webkit_dom_html_object_element_set_hspace;
h$webkit_dom_html_object_element_set_hspace = (function(self,
                                               self_2, val)
                                               {
                                                 self["hspace"] = val;
                                               });
var h$webkit_dom_html_object_element_get_hspace;
h$webkit_dom_html_object_element_get_hspace = (function(self,
                                               self_2)
                                               {
                                                 return self["hspace"];
                                               });
var h$webkit_dom_html_object_element_set_name;
h$webkit_dom_html_object_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_object_element_get_name;
h$webkit_dom_html_object_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_object_element_set_standby;
h$webkit_dom_html_object_element_set_standby = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["standby"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_object_element_get_standby;
h$webkit_dom_html_object_element_get_standby = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["standby"]);
                                                });
var h$webkit_dom_html_object_element_set_use_map;
h$webkit_dom_html_object_element_set_use_map = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["useMap"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_object_element_get_use_map;
h$webkit_dom_html_object_element_get_use_map = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["useMap"]);
                                                });
var h$webkit_dom_html_object_element_set_vspace;
h$webkit_dom_html_object_element_set_vspace = (function(self,
                                               self_2, val)
                                               {
                                                 self["vspace"] = val;
                                               });
var h$webkit_dom_html_object_element_get_vspace;
h$webkit_dom_html_object_element_get_vspace = (function(self,
                                               self_2)
                                               {
                                                 return self["vspace"];
                                               });
var h$webkit_dom_html_object_element_set_width;
h$webkit_dom_html_object_element_set_width = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["width"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_object_element_get_width;
h$webkit_dom_html_object_element_get_width = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["width"]);
                                              });
var h$webkit_dom_html_object_element_get_will_validate;
h$webkit_dom_html_object_element_get_will_validate = (function(self,
                                                      self_2)
                                                      {
                                                        return self["willValidate"];
                                                      });
var h$webkit_dom_html_object_element_get_validity;
h$webkit_dom_html_object_element_get_validity = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["validity"];
                                                 });
var h$webkit_dom_html_object_element_get_validation_message;
h$webkit_dom_html_object_element_get_validation_message = (function(self,
                                                           self_2)
                                                           {
                                                             h$ret1 = 0;
                                                             return h$encodeUtf8(self["validationMessage"]);
                                                           });
var h$webkit_dom_html_object_element_get_content_document;
h$webkit_dom_html_object_element_get_content_document = (function(self,
                                                         self_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return self["contentDocument"];
                                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_mod_element_get_type = (function()
                                          {
                                            return h$g_get_type(HTMLModElement);
                                          });
var h$webkit_dom_html_mod_element_set_cite;
h$webkit_dom_html_mod_element_set_cite = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["cite"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_html_mod_element_get_cite;
h$webkit_dom_html_mod_element_get_cite = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["cite"]);
                                          });
var h$webkit_dom_html_mod_element_set_date_time;
h$webkit_dom_html_mod_element_set_date_time = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["dateTime"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_mod_element_get_date_time;
h$webkit_dom_html_mod_element_get_date_time = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["dateTime"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_meta_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLMetaElement);
                                           });
var h$webkit_dom_html_meta_element_set_content;
h$webkit_dom_html_meta_element_set_content = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["content"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_meta_element_get_content;
h$webkit_dom_html_meta_element_get_content = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["content"]);
                                              });
var h$webkit_dom_html_meta_element_set_http_equiv;
h$webkit_dom_html_meta_element_set_http_equiv = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["httpEquiv"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_meta_element_get_http_equiv;
h$webkit_dom_html_meta_element_get_http_equiv = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["httpEquiv"]);
                                                 });
var h$webkit_dom_html_meta_element_set_name;
h$webkit_dom_html_meta_element_set_name = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["name"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_meta_element_get_name;
h$webkit_dom_html_meta_element_get_name = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["name"]);
                                           });
var h$webkit_dom_html_meta_element_set_scheme;
h$webkit_dom_html_meta_element_set_scheme = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["scheme"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_meta_element_get_scheme;
h$webkit_dom_html_meta_element_get_scheme = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["scheme"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_menu_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLMenuElement);
                                           });
var h$webkit_dom_html_menu_element_set_compact;
h$webkit_dom_html_menu_element_set_compact = (function(self,
                                              self_2, val)
                                              {
                                                self["compact"] = val;
                                              });
var h$webkit_dom_html_menu_element_get_compact;
h$webkit_dom_html_menu_element_get_compact = (function(self,
                                              self_2)
                                              {
                                                return self["compact"];
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_audio_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLAudioElement);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_media_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLMediaElement);
                                            });
var h$webkit_dom_html_media_element_load;
h$webkit_dom_html_media_element_load = (function(self,
                                        self_2)
                                        {
                                          return self["load"]();
                                        });
var h$webkit_dom_html_media_element_can_play_type;
h$webkit_dom_html_media_element_can_play_type = (function(self,
                                                 self_2, type, type_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["canPlayType"](h$decodeUtf8z(type,
                                                   type_2)));
                                                 });
var h$webkit_dom_html_media_element_play;
h$webkit_dom_html_media_element_play = (function(self,
                                        self_2)
                                        {
                                          return self["play"]();
                                        });
var h$webkit_dom_html_media_element_pause;
h$webkit_dom_html_media_element_pause = (function(self,
                                         self_2)
                                         {
                                           return self["pause"]();
                                         });
var h$webkit_dom_html_media_element_get_error;
h$webkit_dom_html_media_element_get_error = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["error"];
                                             });
var h$webkit_dom_html_media_element_set_src;
h$webkit_dom_html_media_element_set_src = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["src"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_media_element_get_src;
h$webkit_dom_html_media_element_get_src = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["src"]);
                                           });
var h$webkit_dom_html_media_element_get_current_src;
h$webkit_dom_html_media_element_get_current_src = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["currentSrc"]);
                                                   });
var h$webkit_dom_html_media_element_get_network_state;
h$webkit_dom_html_media_element_get_network_state = (function(self,
                                                     self_2)
                                                     {
                                                       return self["networkState"];
                                                     });
var h$webkit_dom_html_media_element_set_preload;
h$webkit_dom_html_media_element_set_preload = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["preload"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_media_element_get_preload;
h$webkit_dom_html_media_element_get_preload = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["preload"]);
                                               });
var h$webkit_dom_html_media_element_get_buffered;
h$webkit_dom_html_media_element_get_buffered = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["buffered"];
                                                });
var h$webkit_dom_html_media_element_get_ready_state;
h$webkit_dom_html_media_element_get_ready_state = (function(self,
                                                   self_2)
                                                   {
                                                     return self["readyState"];
                                                   });
var h$webkit_dom_html_media_element_get_seeking;
h$webkit_dom_html_media_element_get_seeking = (function(self,
                                               self_2)
                                               {
                                                 return self["seeking"];
                                               });
var h$webkit_dom_html_media_element_set_current_time;
h$webkit_dom_html_media_element_set_current_time = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["currentTime"] = val;
                                                    });
var h$webkit_dom_html_media_element_get_current_time;
h$webkit_dom_html_media_element_get_current_time = (function(self,
                                                    self_2)
                                                    {
                                                      return self["currentTime"];
                                                    });
var h$webkit_dom_html_media_element_get_initial_time;
h$webkit_dom_html_media_element_get_initial_time = (function(self,
                                                    self_2)
                                                    {
                                                      return self["initialTime"];
                                                    });
var h$webkit_dom_html_media_element_get_start_time;
h$webkit_dom_html_media_element_get_start_time = (function(self,
                                                  self_2)
                                                  {
                                                    return self["startTime"];
                                                  });
var h$webkit_dom_html_media_element_get_duration;
h$webkit_dom_html_media_element_get_duration = (function(self,
                                                self_2)
                                                {
                                                  return self["duration"];
                                                });
var h$webkit_dom_html_media_element_get_paused;
h$webkit_dom_html_media_element_get_paused = (function(self,
                                              self_2)
                                              {
                                                return self["paused"];
                                              });
var h$webkit_dom_html_media_element_set_default_playback_rate;
h$webkit_dom_html_media_element_set_default_playback_rate = (function(self,
                                                             self_2, val)
                                                             {
                                                               self["defaultPlaybackRate"] = val;
                                                             });
var h$webkit_dom_html_media_element_get_default_playback_rate;
h$webkit_dom_html_media_element_get_default_playback_rate = (function(self,
                                                             self_2)
                                                             {
                                                               return self["defaultPlaybackRate"];
                                                             });
var h$webkit_dom_html_media_element_set_playback_rate;
h$webkit_dom_html_media_element_set_playback_rate = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["playbackRate"] = val;
                                                     });
var h$webkit_dom_html_media_element_get_playback_rate;
h$webkit_dom_html_media_element_get_playback_rate = (function(self,
                                                     self_2)
                                                     {
                                                       return self["playbackRate"];
                                                     });
var h$webkit_dom_html_media_element_get_played;
h$webkit_dom_html_media_element_get_played = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["played"];
                                              });
var h$webkit_dom_html_media_element_get_seekable;
h$webkit_dom_html_media_element_get_seekable = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["seekable"];
                                                });
var h$webkit_dom_html_media_element_get_ended;
h$webkit_dom_html_media_element_get_ended = (function(self,
                                             self_2)
                                             {
                                               return self["ended"];
                                             });
var h$webkit_dom_html_media_element_set_autoplay;
h$webkit_dom_html_media_element_set_autoplay = (function(self,
                                                self_2, val)
                                                {
                                                  self["autoplay"] = val;
                                                });
var h$webkit_dom_html_media_element_get_autoplay;
h$webkit_dom_html_media_element_get_autoplay = (function(self,
                                                self_2)
                                                {
                                                  return self["autoplay"];
                                                });
var h$webkit_dom_html_media_element_set_loop;
h$webkit_dom_html_media_element_set_loop = (function(self,
                                            self_2, val)
                                            {
                                              self["loop"] = val;
                                            });
var h$webkit_dom_html_media_element_get_loop;
h$webkit_dom_html_media_element_get_loop = (function(self,
                                            self_2)
                                            {
                                              return self["loop"];
                                            });
var h$webkit_dom_html_media_element_set_controls;
h$webkit_dom_html_media_element_set_controls = (function(self,
                                                self_2, val)
                                                {
                                                  self["controls"] = val;
                                                });
var h$webkit_dom_html_media_element_get_controls;
h$webkit_dom_html_media_element_get_controls = (function(self,
                                                self_2)
                                                {
                                                  return self["controls"];
                                                });
var h$webkit_dom_html_media_element_set_volume;
h$webkit_dom_html_media_element_set_volume = (function(self,
                                              self_2, val)
                                              {
                                                self["volume"] = val;
                                              });
var h$webkit_dom_html_media_element_get_volume;
h$webkit_dom_html_media_element_get_volume = (function(self,
                                              self_2)
                                              {
                                                return self["volume"];
                                              });
var h$webkit_dom_html_media_element_set_muted;
h$webkit_dom_html_media_element_set_muted = (function(self,
                                             self_2, val)
                                             {
                                               self["muted"] = val;
                                             });
var h$webkit_dom_html_media_element_get_muted;
h$webkit_dom_html_media_element_get_muted = (function(self,
                                             self_2)
                                             {
                                               return self["muted"];
                                             });
var h$webkit_dom_html_media_element_set_default_muted;
h$webkit_dom_html_media_element_set_default_muted = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["defaultMuted"] = val;
                                                     });
var h$webkit_dom_html_media_element_get_default_muted;
h$webkit_dom_html_media_element_get_default_muted = (function(self,
                                                     self_2)
                                                     {
                                                       return self["defaultMuted"];
                                                     });
var h$webkit_dom_html_media_element_set_webkit_preserves_pitch;
h$webkit_dom_html_media_element_set_webkit_preserves_pitch = (function(self,
                                                              self_2, val)
                                                              {
                                                                self["webkitPreservesPitch"] = val;
                                                              });
var h$webkit_dom_html_media_element_get_webkit_preserves_pitch;
h$webkit_dom_html_media_element_get_webkit_preserves_pitch = (function(self,
                                                              self_2)
                                                              {
                                                                return self["webkitPreservesPitch"];
                                                              });
var h$webkit_dom_html_media_element_get_webkit_has_closed_captions;
h$webkit_dom_html_media_element_get_webkit_has_closed_captions = (function(self,
                                                                  self_2)
                                                                  {
                                                                    return self["webkitHasClosedCaptions"];
                                                                  });
var h$webkit_dom_html_media_element_set_webkit_closed_captions_visible;
h$webkit_dom_html_media_element_set_webkit_closed_captions_visible = (function(self,
                                                                      self_2,
                                                                      val)
                                                                      {
                                                                        self["webkitClosedCaptionsVisible"] = val;
                                                                      });
var h$webkit_dom_html_media_element_get_webkit_closed_captions_visible;
h$webkit_dom_html_media_element_get_webkit_closed_captions_visible = (function(self,
                                                                      self_2)
                                                                      {
                                                                        return self["webkitClosedCaptionsVisible"];
                                                                      });
var h$webkit_dom_html_media_element_get_webkit_audio_decoded_byte_count;
h$webkit_dom_html_media_element_get_webkit_audio_decoded_byte_count = (function(self,
                                                                       self_2)
                                                                       {
                                                                         return self["webkitAudioDecodedByteCount"];
                                                                       });
var h$webkit_dom_html_media_element_get_webkit_video_decoded_byte_count;
h$webkit_dom_html_media_element_get_webkit_video_decoded_byte_count = (function(self,
                                                                       self_2)
                                                                       {
                                                                         return self["webkitVideoDecodedByteCount"];
                                                                       });
var h$webkit_dom_html_media_element_set_media_group;
h$webkit_dom_html_media_element_set_media_group = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["mediaGroup"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_media_element_get_media_group;
h$webkit_dom_html_media_element_get_media_group = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["mediaGroup"]);
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_marquee_element_get_type = (function()
                                              {
                                                return h$g_get_type(HTMLMarqueeElement);
                                              });
var h$webkit_dom_html_marquee_element_start;
h$webkit_dom_html_marquee_element_start = (function(self,
                                           self_2)
                                           {
                                             return self["start"]();
                                           });
var h$webkit_dom_html_marquee_element_stop;
h$webkit_dom_html_marquee_element_stop = (function(self,
                                          self_2)
                                          {
                                            return self["stop"]();
                                          });
var h$webkit_dom_html_marquee_element_set_behavior;
h$webkit_dom_html_marquee_element_set_behavior = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["behavior"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_marquee_element_get_behavior;
h$webkit_dom_html_marquee_element_get_behavior = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["behavior"]);
                                                  });
var h$webkit_dom_html_marquee_element_set_bg_color;
h$webkit_dom_html_marquee_element_set_bg_color = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["bgColor"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_marquee_element_get_bg_color;
h$webkit_dom_html_marquee_element_get_bg_color = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["bgColor"]);
                                                  });
var h$webkit_dom_html_marquee_element_set_direction;
h$webkit_dom_html_marquee_element_set_direction = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["direction"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_marquee_element_get_direction;
h$webkit_dom_html_marquee_element_get_direction = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["direction"]);
                                                   });
var h$webkit_dom_html_marquee_element_set_height;
h$webkit_dom_html_marquee_element_set_height = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["height"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_marquee_element_get_height;
h$webkit_dom_html_marquee_element_get_height = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["height"]);
                                                });
var h$webkit_dom_html_marquee_element_set_hspace;
h$webkit_dom_html_marquee_element_set_hspace = (function(self,
                                                self_2, val)
                                                {
                                                  self["hspace"] = val;
                                                });
var h$webkit_dom_html_marquee_element_get_hspace;
h$webkit_dom_html_marquee_element_get_hspace = (function(self,
                                                self_2)
                                                {
                                                  return self["hspace"];
                                                });
var h$webkit_dom_html_marquee_element_set_loop;
h$webkit_dom_html_marquee_element_set_loop = (function(self,
                                              self_2, val)
                                              {
                                                self["loop"] = val;
                                              });
var h$webkit_dom_html_marquee_element_get_loop;
h$webkit_dom_html_marquee_element_get_loop = (function(self,
                                              self_2)
                                              {
                                                return self["loop"];
                                              });
var h$webkit_dom_html_marquee_element_set_scroll_amount;
h$webkit_dom_html_marquee_element_set_scroll_amount = (function(self,
                                                       self_2, val)
                                                       {
                                                         self["scrollAmount"] = val;
                                                       });
var h$webkit_dom_html_marquee_element_get_scroll_amount;
h$webkit_dom_html_marquee_element_get_scroll_amount = (function(self,
                                                       self_2)
                                                       {
                                                         return self["scrollAmount"];
                                                       });
var h$webkit_dom_html_marquee_element_set_scroll_delay;
h$webkit_dom_html_marquee_element_set_scroll_delay = (function(self,
                                                      self_2, val)
                                                      {
                                                        self["scrollDelay"] = val;
                                                      });
var h$webkit_dom_html_marquee_element_get_scroll_delay;
h$webkit_dom_html_marquee_element_get_scroll_delay = (function(self,
                                                      self_2)
                                                      {
                                                        return self["scrollDelay"];
                                                      });
var h$webkit_dom_html_marquee_element_set_true_speed;
h$webkit_dom_html_marquee_element_set_true_speed = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["trueSpeed"] = val;
                                                    });
var h$webkit_dom_html_marquee_element_get_true_speed;
h$webkit_dom_html_marquee_element_get_true_speed = (function(self,
                                                    self_2)
                                                    {
                                                      return self["trueSpeed"];
                                                    });
var h$webkit_dom_html_marquee_element_set_vspace;
h$webkit_dom_html_marquee_element_set_vspace = (function(self,
                                                self_2, val)
                                                {
                                                  self["vspace"] = val;
                                                });
var h$webkit_dom_html_marquee_element_get_vspace;
h$webkit_dom_html_marquee_element_get_vspace = (function(self,
                                                self_2)
                                                {
                                                  return self["vspace"];
                                                });
var h$webkit_dom_html_marquee_element_set_width;
h$webkit_dom_html_marquee_element_set_width = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["width"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_marquee_element_get_width;
h$webkit_dom_html_marquee_element_get_width = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["width"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_map_element_get_type = (function()
                                          {
                                            return h$g_get_type(HTMLMapElement);
                                          });
var h$webkit_dom_html_map_element_get_areas;
h$webkit_dom_html_map_element_get_areas = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["areas"];
                                           });
var h$webkit_dom_html_map_element_set_name;
h$webkit_dom_html_map_element_set_name = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["name"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_html_map_element_get_name;
h$webkit_dom_html_map_element_get_name = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["name"]);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_link_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLLinkElement);
                                           });
var h$webkit_dom_html_link_element_set_disabled;
h$webkit_dom_html_link_element_set_disabled = (function(self,
                                               self_2, val)
                                               {
                                                 self["disabled"] = val;
                                               });
var h$webkit_dom_html_link_element_get_disabled;
h$webkit_dom_html_link_element_get_disabled = (function(self,
                                               self_2)
                                               {
                                                 return self["disabled"];
                                               });
var h$webkit_dom_html_link_element_set_charset;
h$webkit_dom_html_link_element_set_charset = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["charset"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_link_element_get_charset;
h$webkit_dom_html_link_element_get_charset = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["charset"]);
                                              });
var h$webkit_dom_html_link_element_set_href;
h$webkit_dom_html_link_element_set_href = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["href"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_link_element_get_href;
h$webkit_dom_html_link_element_get_href = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["href"]);
                                           });
var h$webkit_dom_html_link_element_set_hreflang;
h$webkit_dom_html_link_element_set_hreflang = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["hreflang"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_link_element_get_hreflang;
h$webkit_dom_html_link_element_get_hreflang = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["hreflang"]);
                                               });
var h$webkit_dom_html_link_element_set_media;
h$webkit_dom_html_link_element_set_media = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["media"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_link_element_get_media;
h$webkit_dom_html_link_element_get_media = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["media"]);
                                            });
var h$webkit_dom_html_link_element_set_rel;
h$webkit_dom_html_link_element_set_rel = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["rel"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_html_link_element_get_rel;
h$webkit_dom_html_link_element_get_rel = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["rel"]);
                                          });
var h$webkit_dom_html_link_element_set_rev;
h$webkit_dom_html_link_element_set_rev = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["rev"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_html_link_element_get_rev;
h$webkit_dom_html_link_element_get_rev = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["rev"]);
                                          });
var h$webkit_dom_html_link_element_set_target;
h$webkit_dom_html_link_element_set_target = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["target"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_link_element_get_target;
h$webkit_dom_html_link_element_get_target = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["target"]);
                                             });
var h$webkit_dom_html_link_element_get_sheet;
h$webkit_dom_html_link_element_get_sheet = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["sheet"];
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_htmlli_element_get_type = (function()
                                        {
                                          return h$g_get_type(HTMLLIElement);
                                        });
var h$webkit_dom_htmlli_element_set_value;
h$webkit_dom_htmlli_element_set_value = (function(self,
                                         self_2, val)
                                         {
                                           self["value"] = val;
                                         });
var h$webkit_dom_htmlli_element_get_value;
h$webkit_dom_htmlli_element_get_value = (function(self,
                                         self_2)
                                         {
                                           return self["value"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_legend_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLLegendElement);
                                             });
var h$webkit_dom_html_legend_element_get_form;
h$webkit_dom_html_legend_element_get_form = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["form"];
                                             });
var h$webkit_dom_html_legend_element_set_align;
h$webkit_dom_html_legend_element_set_align = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["align"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_legend_element_get_align;
h$webkit_dom_html_legend_element_get_align = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["align"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_label_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLLabelElement);
                                            });
var h$webkit_dom_html_label_element_get_form;
h$webkit_dom_html_label_element_get_form = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["form"];
                                            });
var h$webkit_dom_html_label_element_set_html_for;
h$webkit_dom_html_label_element_set_html_for = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["htmlFor"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_label_element_get_html_for;
h$webkit_dom_html_label_element_get_html_for = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["htmlFor"]);
                                                });
var h$webkit_dom_html_label_element_get_control;
h$webkit_dom_html_label_element_get_control = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["control"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_keygen_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLKeygenElement);
                                             });
var h$webkit_dom_html_keygen_element_check_validity;
h$webkit_dom_html_keygen_element_check_validity = (function(self,
                                                   self_2)
                                                   {
                                                     return self["checkValidity"]();
                                                   });
var h$webkit_dom_html_keygen_element_set_custom_validity;
h$webkit_dom_html_keygen_element_set_custom_validity = (function(self,
                                                        self_2, error, error_2)
                                                        {
                                                          return self["setCustomValidity"](h$decodeUtf8z(error,
                                                          error_2));
                                                        });
var h$webkit_dom_html_keygen_element_set_autofocus;
h$webkit_dom_html_keygen_element_set_autofocus = (function(self,
                                                  self_2, val)
                                                  {
                                                    self["autofocus"] = val;
                                                  });
var h$webkit_dom_html_keygen_element_get_autofocus;
h$webkit_dom_html_keygen_element_get_autofocus = (function(self,
                                                  self_2)
                                                  {
                                                    return self["autofocus"];
                                                  });
var h$webkit_dom_html_keygen_element_set_challenge;
h$webkit_dom_html_keygen_element_set_challenge = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["challenge"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_keygen_element_get_challenge;
h$webkit_dom_html_keygen_element_get_challenge = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["challenge"]);
                                                  });
var h$webkit_dom_html_keygen_element_set_disabled;
h$webkit_dom_html_keygen_element_set_disabled = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["disabled"] = val;
                                                 });
var h$webkit_dom_html_keygen_element_get_disabled;
h$webkit_dom_html_keygen_element_get_disabled = (function(self,
                                                 self_2)
                                                 {
                                                   return self["disabled"];
                                                 });
var h$webkit_dom_html_keygen_element_get_form;
h$webkit_dom_html_keygen_element_get_form = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["form"];
                                             });
var h$webkit_dom_html_keygen_element_set_keytype;
h$webkit_dom_html_keygen_element_set_keytype = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["keytype"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_keygen_element_get_keytype;
h$webkit_dom_html_keygen_element_get_keytype = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["keytype"]);
                                                });
var h$webkit_dom_html_keygen_element_set_name;
h$webkit_dom_html_keygen_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_keygen_element_get_name;
h$webkit_dom_html_keygen_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_keygen_element_get_will_validate;
h$webkit_dom_html_keygen_element_get_will_validate = (function(self,
                                                      self_2)
                                                      {
                                                        return self["willValidate"];
                                                      });
var h$webkit_dom_html_keygen_element_get_validity;
h$webkit_dom_html_keygen_element_get_validity = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["validity"];
                                                 });
var h$webkit_dom_html_keygen_element_get_validation_message;
h$webkit_dom_html_keygen_element_get_validation_message = (function(self,
                                                           self_2)
                                                           {
                                                             h$ret1 = 0;
                                                             return h$encodeUtf8(self["validationMessage"]);
                                                           });
var h$webkit_dom_html_keygen_element_get_labels;
h$webkit_dom_html_keygen_element_get_labels = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["labels"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_input_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLInputElement);
                                            });
var h$webkit_dom_html_input_element_step_up;
h$webkit_dom_html_input_element_step_up = (function(self,
                                           self_2, n)
                                           {
                                             return self["stepUp"](n);
                                           });
var h$webkit_dom_html_input_element_step_down;
h$webkit_dom_html_input_element_step_down = (function(self,
                                             self_2, n)
                                             {
                                               return self["stepDown"](n);
                                             });
var h$webkit_dom_html_input_element_check_validity;
h$webkit_dom_html_input_element_check_validity = (function(self,
                                                  self_2)
                                                  {
                                                    return self["checkValidity"]();
                                                  });
var h$webkit_dom_html_input_element_set_custom_validity;
h$webkit_dom_html_input_element_set_custom_validity = (function(self,
                                                       self_2, error, error_2)
                                                       {
                                                         return self["setCustomValidity"](h$decodeUtf8z(error,
                                                         error_2));
                                                       });
var h$webkit_dom_html_input_element_select;
h$webkit_dom_html_input_element_select = (function(self,
                                          self_2)
                                          {
                                            return self["select"]();
                                          });
var h$webkit_dom_html_input_element_set_value_for_user;
h$webkit_dom_html_input_element_set_value_for_user = (function(self,
                                                      self_2, value, value_2)
                                                      {
                                                        return self["setValueForUser"](h$decodeUtf8z(value,
                                                        value_2));
                                                      });
var h$webkit_dom_html_input_element_set_accept;
h$webkit_dom_html_input_element_set_accept = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["accept"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_input_element_get_accept;
h$webkit_dom_html_input_element_get_accept = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["accept"]);
                                              });
var h$webkit_dom_html_input_element_set_alt;
h$webkit_dom_html_input_element_set_alt = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["alt"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_input_element_get_alt;
h$webkit_dom_html_input_element_get_alt = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["alt"]);
                                           });
var h$webkit_dom_html_input_element_set_autocomplete;
h$webkit_dom_html_input_element_set_autocomplete = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["autocomplete"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_input_element_get_autocomplete;
h$webkit_dom_html_input_element_get_autocomplete = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["autocomplete"]);
                                                    });
var h$webkit_dom_html_input_element_set_autofocus;
h$webkit_dom_html_input_element_set_autofocus = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["autofocus"] = val;
                                                 });
var h$webkit_dom_html_input_element_get_autofocus;
h$webkit_dom_html_input_element_get_autofocus = (function(self,
                                                 self_2)
                                                 {
                                                   return self["autofocus"];
                                                 });
var h$webkit_dom_html_input_element_set_default_checked;
h$webkit_dom_html_input_element_set_default_checked = (function(self,
                                                       self_2, val)
                                                       {
                                                         self["defaultChecked"] = val;
                                                       });
var h$webkit_dom_html_input_element_get_default_checked;
h$webkit_dom_html_input_element_get_default_checked = (function(self,
                                                       self_2)
                                                       {
                                                         return self["defaultChecked"];
                                                       });
var h$webkit_dom_html_input_element_set_checked;
h$webkit_dom_html_input_element_set_checked = (function(self,
                                               self_2, val)
                                               {
                                                 self["checked"] = val;
                                               });
var h$webkit_dom_html_input_element_get_checked;
h$webkit_dom_html_input_element_get_checked = (function(self,
                                               self_2)
                                               {
                                                 return self["checked"];
                                               });
var h$webkit_dom_html_input_element_set_dir_name;
h$webkit_dom_html_input_element_set_dir_name = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["dirName"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_input_element_get_dir_name;
h$webkit_dom_html_input_element_get_dir_name = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["dirName"]);
                                                });
var h$webkit_dom_html_input_element_set_disabled;
h$webkit_dom_html_input_element_set_disabled = (function(self,
                                                self_2, val)
                                                {
                                                  self["disabled"] = val;
                                                });
var h$webkit_dom_html_input_element_get_disabled;
h$webkit_dom_html_input_element_get_disabled = (function(self,
                                                self_2)
                                                {
                                                  return self["disabled"];
                                                });
var h$webkit_dom_html_input_element_get_form;
h$webkit_dom_html_input_element_get_form = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["form"];
                                            });
var h$webkit_dom_html_input_element_set_files;
h$webkit_dom_html_input_element_set_files = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["files"] = val;
                                             });
var h$webkit_dom_html_input_element_get_files;
h$webkit_dom_html_input_element_get_files = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["files"];
                                             });
var h$webkit_dom_html_input_element_set_form_action;
h$webkit_dom_html_input_element_set_form_action = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["formAction"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_input_element_get_form_action;
h$webkit_dom_html_input_element_get_form_action = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["formAction"]);
                                                   });
var h$webkit_dom_html_input_element_set_form_enctype;
h$webkit_dom_html_input_element_set_form_enctype = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["formEnctype"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_input_element_get_form_enctype;
h$webkit_dom_html_input_element_get_form_enctype = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["formEnctype"]);
                                                    });
var h$webkit_dom_html_input_element_set_form_method;
h$webkit_dom_html_input_element_set_form_method = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["formMethod"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_input_element_get_form_method;
h$webkit_dom_html_input_element_get_form_method = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["formMethod"]);
                                                   });
var h$webkit_dom_html_input_element_set_form_no_validate;
h$webkit_dom_html_input_element_set_form_no_validate = (function(self,
                                                        self_2, val)
                                                        {
                                                          self["formNoValidate"] = val;
                                                        });
var h$webkit_dom_html_input_element_get_form_no_validate;
h$webkit_dom_html_input_element_get_form_no_validate = (function(self,
                                                        self_2)
                                                        {
                                                          return self["formNoValidate"];
                                                        });
var h$webkit_dom_html_input_element_set_form_target;
h$webkit_dom_html_input_element_set_form_target = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["formTarget"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_input_element_get_form_target;
h$webkit_dom_html_input_element_get_form_target = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["formTarget"]);
                                                   });
var h$webkit_dom_html_input_element_set_height;
h$webkit_dom_html_input_element_set_height = (function(self,
                                              self_2, val)
                                              {
                                                self["height"] = val;
                                              });
var h$webkit_dom_html_input_element_get_height;
h$webkit_dom_html_input_element_get_height = (function(self,
                                              self_2)
                                              {
                                                return self["height"];
                                              });
var h$webkit_dom_html_input_element_set_indeterminate;
h$webkit_dom_html_input_element_set_indeterminate = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["indeterminate"] = val;
                                                     });
var h$webkit_dom_html_input_element_get_indeterminate;
h$webkit_dom_html_input_element_get_indeterminate = (function(self,
                                                     self_2)
                                                     {
                                                       return self["indeterminate"];
                                                     });
var h$webkit_dom_html_input_element_get_list;
h$webkit_dom_html_input_element_get_list = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["list"];
                                            });
var h$webkit_dom_html_input_element_set_max;
h$webkit_dom_html_input_element_set_max = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["max"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_input_element_get_max;
h$webkit_dom_html_input_element_get_max = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["max"]);
                                           });
var h$webkit_dom_html_input_element_set_max_length;
h$webkit_dom_html_input_element_set_max_length = (function(self,
                                                  self_2, val)
                                                  {
                                                    self["maxLength"] = val;
                                                  });
var h$webkit_dom_html_input_element_get_max_length;
h$webkit_dom_html_input_element_get_max_length = (function(self,
                                                  self_2)
                                                  {
                                                    return self["maxLength"];
                                                  });
var h$webkit_dom_html_input_element_set_min;
h$webkit_dom_html_input_element_set_min = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["min"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_input_element_get_min;
h$webkit_dom_html_input_element_get_min = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["min"]);
                                           });
var h$webkit_dom_html_input_element_set_multiple;
h$webkit_dom_html_input_element_set_multiple = (function(self,
                                                self_2, val)
                                                {
                                                  self["multiple"] = val;
                                                });
var h$webkit_dom_html_input_element_get_multiple;
h$webkit_dom_html_input_element_get_multiple = (function(self,
                                                self_2)
                                                {
                                                  return self["multiple"];
                                                });
var h$webkit_dom_html_input_element_set_name;
h$webkit_dom_html_input_element_set_name = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["name"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_input_element_get_name;
h$webkit_dom_html_input_element_get_name = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["name"]);
                                            });
var h$webkit_dom_html_input_element_set_pattern;
h$webkit_dom_html_input_element_set_pattern = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["pattern"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_input_element_get_pattern;
h$webkit_dom_html_input_element_get_pattern = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["pattern"]);
                                               });
var h$webkit_dom_html_input_element_set_placeholder;
h$webkit_dom_html_input_element_set_placeholder = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["placeholder"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_input_element_get_placeholder;
h$webkit_dom_html_input_element_get_placeholder = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["placeholder"]);
                                                   });
var h$webkit_dom_html_input_element_set_read_only;
h$webkit_dom_html_input_element_set_read_only = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["readOnly"] = val;
                                                 });
var h$webkit_dom_html_input_element_get_read_only;
h$webkit_dom_html_input_element_get_read_only = (function(self,
                                                 self_2)
                                                 {
                                                   return self["readOnly"];
                                                 });
var h$webkit_dom_html_input_element_set_required;
h$webkit_dom_html_input_element_set_required = (function(self,
                                                self_2, val)
                                                {
                                                  self["required"] = val;
                                                });
var h$webkit_dom_html_input_element_get_required;
h$webkit_dom_html_input_element_get_required = (function(self,
                                                self_2)
                                                {
                                                  return self["required"];
                                                });
var h$webkit_dom_html_input_element_set_size;
h$webkit_dom_html_input_element_set_size = (function(self,
                                            self_2, val)
                                            {
                                              self["size"] = val;
                                            });
var h$webkit_dom_html_input_element_get_size;
h$webkit_dom_html_input_element_get_size = (function(self,
                                            self_2)
                                            {
                                              return self["size"];
                                            });
var h$webkit_dom_html_input_element_set_src;
h$webkit_dom_html_input_element_set_src = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["src"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_input_element_get_src;
h$webkit_dom_html_input_element_get_src = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["src"]);
                                           });
var h$webkit_dom_html_input_element_set_step;
h$webkit_dom_html_input_element_set_step = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["step"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_input_element_get_step;
h$webkit_dom_html_input_element_get_step = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["step"]);
                                            });
var h$webkit_dom_html_input_element_set_default_value;
h$webkit_dom_html_input_element_set_default_value = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["defaultValue"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_input_element_get_default_value;
h$webkit_dom_html_input_element_get_default_value = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["defaultValue"]);
                                                     });
var h$webkit_dom_html_input_element_set_value;
h$webkit_dom_html_input_element_set_value = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["value"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_input_element_get_value;
h$webkit_dom_html_input_element_get_value = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["value"]);
                                             });
var h$webkit_dom_html_input_element_set_value_as_number;
h$webkit_dom_html_input_element_set_value_as_number = (function(self,
                                                       self_2, val)
                                                       {
                                                         self["valueAsNumber"] = val;
                                                       });
var h$webkit_dom_html_input_element_get_value_as_number;
h$webkit_dom_html_input_element_get_value_as_number = (function(self,
                                                       self_2)
                                                       {
                                                         return self["valueAsNumber"];
                                                       });
var h$webkit_dom_html_input_element_set_width;
h$webkit_dom_html_input_element_set_width = (function(self,
                                             self_2, val)
                                             {
                                               self["width"] = val;
                                             });
var h$webkit_dom_html_input_element_get_width;
h$webkit_dom_html_input_element_get_width = (function(self,
                                             self_2)
                                             {
                                               return self["width"];
                                             });
var h$webkit_dom_html_input_element_get_will_validate;
h$webkit_dom_html_input_element_get_will_validate = (function(self,
                                                     self_2)
                                                     {
                                                       return self["willValidate"];
                                                     });
var h$webkit_dom_html_input_element_get_validity;
h$webkit_dom_html_input_element_get_validity = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["validity"];
                                                });
var h$webkit_dom_html_input_element_get_validation_message;
h$webkit_dom_html_input_element_get_validation_message = (function(self,
                                                          self_2)
                                                          {
                                                            h$ret1 = 0;
                                                            return h$encodeUtf8(self["validationMessage"]);
                                                          });
var h$webkit_dom_html_input_element_get_labels;
h$webkit_dom_html_input_element_get_labels = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["labels"];
                                              });
var h$webkit_dom_html_input_element_set_align;
h$webkit_dom_html_input_element_set_align = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["align"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_input_element_get_align;
h$webkit_dom_html_input_element_get_align = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["align"]);
                                             });
var h$webkit_dom_html_input_element_set_webkitdirectory;
h$webkit_dom_html_input_element_set_webkitdirectory = (function(self,
                                                       self_2, val)
                                                       {
                                                         self["webkitdirectory"] = val;
                                                       });
var h$webkit_dom_html_input_element_get_webkitdirectory;
h$webkit_dom_html_input_element_get_webkitdirectory = (function(self,
                                                       self_2)
                                                       {
                                                         return self["webkitdirectory"];
                                                       });
var h$webkit_dom_html_input_element_set_use_map;
h$webkit_dom_html_input_element_set_use_map = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["useMap"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_input_element_get_use_map;
h$webkit_dom_html_input_element_get_use_map = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["useMap"]);
                                               });
var h$webkit_dom_html_input_element_set_incremental;
h$webkit_dom_html_input_element_set_incremental = (function(self,
                                                   self_2, val)
                                                   {
                                                     self["incremental"] = val;
                                                   });
var h$webkit_dom_html_input_element_get_incremental;
h$webkit_dom_html_input_element_get_incremental = (function(self,
                                                   self_2)
                                                   {
                                                     return self["incremental"];
                                                   });
var h$webkit_dom_html_input_element_set_webkit_speech;
h$webkit_dom_html_input_element_set_webkit_speech = (function(self,
                                                     self_2, val)
                                                     {
                                                       self["webkitSpeech"] = val;
                                                     });
var h$webkit_dom_html_input_element_get_webkit_speech;
h$webkit_dom_html_input_element_get_webkit_speech = (function(self,
                                                     self_2)
                                                     {
                                                       return self["webkitSpeech"];
                                                     });
var h$webkit_dom_html_input_element_set_webkit_grammar;
h$webkit_dom_html_input_element_set_webkit_grammar = (function(self,
                                                      self_2, val)
                                                      {
                                                        self["webkitGrammar"] = val;
                                                      });
var h$webkit_dom_html_input_element_get_webkit_grammar;
h$webkit_dom_html_input_element_get_webkit_grammar = (function(self,
                                                      self_2)
                                                      {
                                                        return self["webkitGrammar"];
                                                      });
var h$webkit_dom_html_input_element_set_onwebkitspeechchange;
h$webkit_dom_html_input_element_set_onwebkitspeechchange = (function(self,
                                                            self_2, val, val_2)
                                                            {
                                                              self["onwebkitspeechchange"] = val;
                                                            });
var h$webkit_dom_html_input_element_get_onwebkitspeechchange;
h$webkit_dom_html_input_element_get_onwebkitspeechchange = (function(self,
                                                            self_2)
                                                            {
                                                              h$ret1 = 0;
                                                              return self["onwebkitspeechchange"];
                                                            });
var h$webkit_dom_html_input_element_set_capture;
h$webkit_dom_html_input_element_set_capture = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["capture"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_input_element_get_capture;
h$webkit_dom_html_input_element_get_capture = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["capture"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_image_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLImageElement);
                                            });
var h$webkit_dom_html_image_element_set_name;
h$webkit_dom_html_image_element_set_name = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["name"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_image_element_get_name;
h$webkit_dom_html_image_element_get_name = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["name"]);
                                            });
var h$webkit_dom_html_image_element_set_align;
h$webkit_dom_html_image_element_set_align = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["align"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_image_element_get_align;
h$webkit_dom_html_image_element_get_align = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["align"]);
                                             });
var h$webkit_dom_html_image_element_set_alt;
h$webkit_dom_html_image_element_set_alt = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["alt"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_image_element_get_alt;
h$webkit_dom_html_image_element_get_alt = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["alt"]);
                                           });
var h$webkit_dom_html_image_element_set_border;
h$webkit_dom_html_image_element_set_border = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["border"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_image_element_get_border;
h$webkit_dom_html_image_element_get_border = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["border"]);
                                              });
var h$webkit_dom_html_image_element_set_cross_origin;
h$webkit_dom_html_image_element_set_cross_origin = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["crossOrigin"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_image_element_get_cross_origin;
h$webkit_dom_html_image_element_get_cross_origin = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["crossOrigin"]);
                                                    });
var h$webkit_dom_html_image_element_set_height;
h$webkit_dom_html_image_element_set_height = (function(self,
                                              self_2, val)
                                              {
                                                self["height"] = val;
                                              });
var h$webkit_dom_html_image_element_get_height;
h$webkit_dom_html_image_element_get_height = (function(self,
                                              self_2)
                                              {
                                                return self["height"];
                                              });
var h$webkit_dom_html_image_element_set_hspace;
h$webkit_dom_html_image_element_set_hspace = (function(self,
                                              self_2, val)
                                              {
                                                self["hspace"] = val;
                                              });
var h$webkit_dom_html_image_element_get_hspace;
h$webkit_dom_html_image_element_get_hspace = (function(self,
                                              self_2)
                                              {
                                                return self["hspace"];
                                              });
var h$webkit_dom_html_image_element_set_is_map;
h$webkit_dom_html_image_element_set_is_map = (function(self,
                                              self_2, val)
                                              {
                                                self["isMap"] = val;
                                              });
var h$webkit_dom_html_image_element_get_is_map;
h$webkit_dom_html_image_element_get_is_map = (function(self,
                                              self_2)
                                              {
                                                return self["isMap"];
                                              });
var h$webkit_dom_html_image_element_set_long_desc;
h$webkit_dom_html_image_element_set_long_desc = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["longDesc"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_image_element_get_long_desc;
h$webkit_dom_html_image_element_get_long_desc = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["longDesc"]);
                                                 });
var h$webkit_dom_html_image_element_set_src;
h$webkit_dom_html_image_element_set_src = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["src"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_image_element_get_src;
h$webkit_dom_html_image_element_get_src = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["src"]);
                                           });
var h$webkit_dom_html_image_element_set_use_map;
h$webkit_dom_html_image_element_set_use_map = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["useMap"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_image_element_get_use_map;
h$webkit_dom_html_image_element_get_use_map = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["useMap"]);
                                               });
var h$webkit_dom_html_image_element_set_vspace;
h$webkit_dom_html_image_element_set_vspace = (function(self,
                                              self_2, val)
                                              {
                                                self["vspace"] = val;
                                              });
var h$webkit_dom_html_image_element_get_vspace;
h$webkit_dom_html_image_element_get_vspace = (function(self,
                                              self_2)
                                              {
                                                return self["vspace"];
                                              });
var h$webkit_dom_html_image_element_set_width;
h$webkit_dom_html_image_element_set_width = (function(self,
                                             self_2, val)
                                             {
                                               self["width"] = val;
                                             });
var h$webkit_dom_html_image_element_get_width;
h$webkit_dom_html_image_element_get_width = (function(self,
                                             self_2)
                                             {
                                               return self["width"];
                                             });
var h$webkit_dom_html_image_element_get_complete;
h$webkit_dom_html_image_element_get_complete = (function(self,
                                                self_2)
                                                {
                                                  return self["complete"];
                                                });
var h$webkit_dom_html_image_element_set_lowsrc;
h$webkit_dom_html_image_element_set_lowsrc = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["lowsrc"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_image_element_get_lowsrc;
h$webkit_dom_html_image_element_get_lowsrc = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["lowsrc"]);
                                              });
var h$webkit_dom_html_image_element_get_natural_height;
h$webkit_dom_html_image_element_get_natural_height = (function(self,
                                                      self_2)
                                                      {
                                                        return self["naturalHeight"];
                                                      });
var h$webkit_dom_html_image_element_get_natural_width;
h$webkit_dom_html_image_element_get_natural_width = (function(self,
                                                     self_2)
                                                     {
                                                       return self["naturalWidth"];
                                                     });
var h$webkit_dom_html_image_element_get_x;
h$webkit_dom_html_image_element_get_x = (function(self,
                                         self_2)
                                         {
                                           return self["x"];
                                         });
var h$webkit_dom_html_image_element_get_y;
h$webkit_dom_html_image_element_get_y = (function(self,
                                         self_2)
                                         {
                                           return self["y"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_iframe_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLIFrameElement);
                                             });
var h$webkit_dom_html_iframe_element_set_align;
h$webkit_dom_html_iframe_element_set_align = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["align"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_iframe_element_get_align;
h$webkit_dom_html_iframe_element_get_align = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["align"]);
                                              });
var h$webkit_dom_html_iframe_element_set_frame_border;
h$webkit_dom_html_iframe_element_set_frame_border = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["frameBorder"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_iframe_element_get_frame_border;
h$webkit_dom_html_iframe_element_get_frame_border = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["frameBorder"]);
                                                     });
var h$webkit_dom_html_iframe_element_set_height;
h$webkit_dom_html_iframe_element_set_height = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["height"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_iframe_element_get_height;
h$webkit_dom_html_iframe_element_get_height = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["height"]);
                                               });
var h$webkit_dom_html_iframe_element_set_long_desc;
h$webkit_dom_html_iframe_element_set_long_desc = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["longDesc"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_iframe_element_get_long_desc;
h$webkit_dom_html_iframe_element_get_long_desc = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["longDesc"]);
                                                  });
var h$webkit_dom_html_iframe_element_set_margin_height;
h$webkit_dom_html_iframe_element_set_margin_height = (function(self,
                                                      self_2, val, val_2)
                                                      {
                                                        self["marginHeight"] = h$decodeUtf8z(val,
                                                        val_2);
                                                      });
var h$webkit_dom_html_iframe_element_get_margin_height;
h$webkit_dom_html_iframe_element_get_margin_height = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return h$encodeUtf8(self["marginHeight"]);
                                                      });
var h$webkit_dom_html_iframe_element_set_margin_width;
h$webkit_dom_html_iframe_element_set_margin_width = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["marginWidth"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_iframe_element_get_margin_width;
h$webkit_dom_html_iframe_element_get_margin_width = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["marginWidth"]);
                                                     });
var h$webkit_dom_html_iframe_element_set_name;
h$webkit_dom_html_iframe_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_iframe_element_get_name;
h$webkit_dom_html_iframe_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_iframe_element_set_sandbox;
h$webkit_dom_html_iframe_element_set_sandbox = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["sandbox"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_iframe_element_get_sandbox;
h$webkit_dom_html_iframe_element_get_sandbox = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["sandbox"]);
                                                });
var h$webkit_dom_html_iframe_element_set_seamless;
h$webkit_dom_html_iframe_element_set_seamless = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["seamless"] = val;
                                                 });
var h$webkit_dom_html_iframe_element_get_seamless;
h$webkit_dom_html_iframe_element_get_seamless = (function(self,
                                                 self_2)
                                                 {
                                                   return self["seamless"];
                                                 });
var h$webkit_dom_html_iframe_element_set_scrolling;
h$webkit_dom_html_iframe_element_set_scrolling = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["scrolling"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_iframe_element_get_scrolling;
h$webkit_dom_html_iframe_element_get_scrolling = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["scrolling"]);
                                                  });
var h$webkit_dom_html_iframe_element_set_src;
h$webkit_dom_html_iframe_element_set_src = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["src"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_iframe_element_get_src;
h$webkit_dom_html_iframe_element_get_src = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["src"]);
                                            });
var h$webkit_dom_html_iframe_element_set_srcdoc;
h$webkit_dom_html_iframe_element_set_srcdoc = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["srcdoc"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_iframe_element_get_srcdoc;
h$webkit_dom_html_iframe_element_get_srcdoc = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["srcdoc"]);
                                               });
var h$webkit_dom_html_iframe_element_set_width;
h$webkit_dom_html_iframe_element_set_width = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["width"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_iframe_element_get_width;
h$webkit_dom_html_iframe_element_get_width = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["width"]);
                                              });
var h$webkit_dom_html_iframe_element_get_content_document;
h$webkit_dom_html_iframe_element_get_content_document = (function(self,
                                                         self_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return self["contentDocument"];
                                                         });
var h$webkit_dom_html_iframe_element_get_content_window;
h$webkit_dom_html_iframe_element_get_content_window = (function(self,
                                                       self_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return self["contentWindow"];
                                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_html_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLHtmlElement);
                                           });
var h$webkit_dom_html_html_element_set_version;
h$webkit_dom_html_html_element_set_version = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["version"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_html_element_get_version;
h$webkit_dom_html_html_element_get_version = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["version"]);
                                              });
var h$webkit_dom_html_html_element_set_manifest;
h$webkit_dom_html_html_element_set_manifest = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["manifest"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_html_element_get_manifest;
h$webkit_dom_html_html_element_get_manifest = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["manifest"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_htmlhr_element_get_type = (function()
                                        {
                                          return h$g_get_type(HTMLHRElement);
                                        });
var h$webkit_dom_htmlhr_element_set_align;
h$webkit_dom_htmlhr_element_set_align = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["align"] = h$decodeUtf8z(val,
                                           val_2);
                                         });
var h$webkit_dom_htmlhr_element_get_align;
h$webkit_dom_htmlhr_element_get_align = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["align"]);
                                         });
var h$webkit_dom_htmlhr_element_set_no_shade;
h$webkit_dom_htmlhr_element_set_no_shade = (function(self,
                                            self_2, val)
                                            {
                                              self["noShade"] = val;
                                            });
var h$webkit_dom_htmlhr_element_get_no_shade;
h$webkit_dom_htmlhr_element_get_no_shade = (function(self,
                                            self_2)
                                            {
                                              return self["noShade"];
                                            });
var h$webkit_dom_htmlhr_element_set_size;
h$webkit_dom_htmlhr_element_set_size = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["size"] = h$decodeUtf8z(val,
                                          val_2);
                                        });
var h$webkit_dom_htmlhr_element_get_size;
h$webkit_dom_htmlhr_element_get_size = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return h$encodeUtf8(self["size"]);
                                        });
var h$webkit_dom_htmlhr_element_set_width;
h$webkit_dom_htmlhr_element_set_width = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["width"] = h$decodeUtf8z(val,
                                           val_2);
                                         });
var h$webkit_dom_htmlhr_element_get_width;
h$webkit_dom_htmlhr_element_get_width = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["width"]);
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_heading_element_get_type = (function()
                                              {
                                                return h$g_get_type(HTMLHeadingElement);
                                              });
var h$webkit_dom_html_heading_element_set_align;
h$webkit_dom_html_heading_element_set_align = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["align"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_heading_element_get_align;
h$webkit_dom_html_heading_element_get_align = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["align"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_head_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLHeadElement);
                                           });
var h$webkit_dom_html_head_element_set_profile;
h$webkit_dom_html_head_element_set_profile = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["profile"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_head_element_get_profile;
h$webkit_dom_html_head_element_get_profile = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["profile"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_frame_set_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLFrameSetElement);
                                                });
var h$webkit_dom_html_frame_set_element_set_cols;
h$webkit_dom_html_frame_set_element_set_cols = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["cols"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_frame_set_element_get_cols;
h$webkit_dom_html_frame_set_element_get_cols = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["cols"]);
                                                });
var h$webkit_dom_html_frame_set_element_set_rows;
h$webkit_dom_html_frame_set_element_set_rows = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["rows"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_frame_set_element_get_rows;
h$webkit_dom_html_frame_set_element_get_rows = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["rows"]);
                                                });
var h$webkit_dom_html_frame_set_element_set_onbeforeunload;
h$webkit_dom_html_frame_set_element_set_onbeforeunload = (function(self,
                                                          self_2, val, val_2)
                                                          {
                                                            self["onbeforeunload"] = val;
                                                          });
var h$webkit_dom_html_frame_set_element_get_onbeforeunload;
h$webkit_dom_html_frame_set_element_get_onbeforeunload = (function(self,
                                                          self_2)
                                                          {
                                                            h$ret1 = 0;
                                                            return self["onbeforeunload"];
                                                          });
var h$webkit_dom_html_frame_set_element_set_onhashchange;
h$webkit_dom_html_frame_set_element_set_onhashchange = (function(self,
                                                        self_2, val, val_2)
                                                        {
                                                          self["onhashchange"] = val;
                                                        });
var h$webkit_dom_html_frame_set_element_get_onhashchange;
h$webkit_dom_html_frame_set_element_get_onhashchange = (function(self,
                                                        self_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["onhashchange"];
                                                        });
var h$webkit_dom_html_frame_set_element_set_onmessage;
h$webkit_dom_html_frame_set_element_set_onmessage = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onmessage"] = val;
                                                     });
var h$webkit_dom_html_frame_set_element_get_onmessage;
h$webkit_dom_html_frame_set_element_get_onmessage = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onmessage"];
                                                     });
var h$webkit_dom_html_frame_set_element_set_onoffline;
h$webkit_dom_html_frame_set_element_set_onoffline = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onoffline"] = val;
                                                     });
var h$webkit_dom_html_frame_set_element_get_onoffline;
h$webkit_dom_html_frame_set_element_get_onoffline = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onoffline"];
                                                     });
var h$webkit_dom_html_frame_set_element_set_ononline;
h$webkit_dom_html_frame_set_element_set_ononline = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["ononline"] = val;
                                                    });
var h$webkit_dom_html_frame_set_element_get_ononline;
h$webkit_dom_html_frame_set_element_get_ononline = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["ononline"];
                                                    });
var h$webkit_dom_html_frame_set_element_set_onpopstate;
h$webkit_dom_html_frame_set_element_set_onpopstate = (function(self,
                                                      self_2, val, val_2)
                                                      {
                                                        self["onpopstate"] = val;
                                                      });
var h$webkit_dom_html_frame_set_element_get_onpopstate;
h$webkit_dom_html_frame_set_element_get_onpopstate = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["onpopstate"];
                                                      });
var h$webkit_dom_html_frame_set_element_set_onresize;
h$webkit_dom_html_frame_set_element_set_onresize = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["onresize"] = val;
                                                    });
var h$webkit_dom_html_frame_set_element_get_onresize;
h$webkit_dom_html_frame_set_element_get_onresize = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["onresize"];
                                                    });
var h$webkit_dom_html_frame_set_element_set_onstorage;
h$webkit_dom_html_frame_set_element_set_onstorage = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onstorage"] = val;
                                                     });
var h$webkit_dom_html_frame_set_element_get_onstorage;
h$webkit_dom_html_frame_set_element_get_onstorage = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onstorage"];
                                                     });
var h$webkit_dom_html_frame_set_element_set_onunload;
h$webkit_dom_html_frame_set_element_set_onunload = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["onunload"] = val;
                                                    });
var h$webkit_dom_html_frame_set_element_get_onunload;
h$webkit_dom_html_frame_set_element_get_onunload = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["onunload"];
                                                    });
var h$webkit_dom_html_frame_set_element_set_onorientationchange;
h$webkit_dom_html_frame_set_element_set_onorientationchange = (function(self,
                                                               self_2, val,
                                                               val_2)
                                                               {
                                                                 self["onorientationchange"] = val;
                                                               });
var h$webkit_dom_html_frame_set_element_get_onorientationchange;
h$webkit_dom_html_frame_set_element_get_onorientationchange = (function(self,
                                                               self_2)
                                                               {
                                                                 h$ret1 = 0;
                                                                 return self["onorientationchange"];
                                                               });
var h$webkit_dom_html_frame_set_element_set_onblur;
h$webkit_dom_html_frame_set_element_set_onblur = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["onblur"] = val;
                                                  });
var h$webkit_dom_html_frame_set_element_get_onblur;
h$webkit_dom_html_frame_set_element_get_onblur = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["onblur"];
                                                  });
var h$webkit_dom_html_frame_set_element_set_onerror;
h$webkit_dom_html_frame_set_element_set_onerror = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["onerror"] = val;
                                                   });
var h$webkit_dom_html_frame_set_element_get_onerror;
h$webkit_dom_html_frame_set_element_get_onerror = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["onerror"];
                                                   });
var h$webkit_dom_html_frame_set_element_set_onfocus;
h$webkit_dom_html_frame_set_element_set_onfocus = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["onfocus"] = val;
                                                   });
var h$webkit_dom_html_frame_set_element_get_onfocus;
h$webkit_dom_html_frame_set_element_get_onfocus = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["onfocus"];
                                                   });
var h$webkit_dom_html_frame_set_element_set_onload;
h$webkit_dom_html_frame_set_element_set_onload = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["onload"] = val;
                                                  });
var h$webkit_dom_html_frame_set_element_get_onload;
h$webkit_dom_html_frame_set_element_get_onload = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["onload"];
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_frame_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLFrameElement);
                                            });
var h$webkit_dom_html_frame_element_set_frame_border;
h$webkit_dom_html_frame_element_set_frame_border = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["frameBorder"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_frame_element_get_frame_border;
h$webkit_dom_html_frame_element_get_frame_border = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["frameBorder"]);
                                                    });
var h$webkit_dom_html_frame_element_set_long_desc;
h$webkit_dom_html_frame_element_set_long_desc = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["longDesc"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_frame_element_get_long_desc;
h$webkit_dom_html_frame_element_get_long_desc = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["longDesc"]);
                                                 });
var h$webkit_dom_html_frame_element_set_margin_height;
h$webkit_dom_html_frame_element_set_margin_height = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["marginHeight"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_frame_element_get_margin_height;
h$webkit_dom_html_frame_element_get_margin_height = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["marginHeight"]);
                                                     });
var h$webkit_dom_html_frame_element_set_margin_width;
h$webkit_dom_html_frame_element_set_margin_width = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["marginWidth"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_frame_element_get_margin_width;
h$webkit_dom_html_frame_element_get_margin_width = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["marginWidth"]);
                                                    });
var h$webkit_dom_html_frame_element_set_name;
h$webkit_dom_html_frame_element_set_name = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["name"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_frame_element_get_name;
h$webkit_dom_html_frame_element_get_name = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["name"]);
                                            });
var h$webkit_dom_html_frame_element_set_no_resize;
h$webkit_dom_html_frame_element_set_no_resize = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["noResize"] = val;
                                                 });
var h$webkit_dom_html_frame_element_get_no_resize;
h$webkit_dom_html_frame_element_get_no_resize = (function(self,
                                                 self_2)
                                                 {
                                                   return self["noResize"];
                                                 });
var h$webkit_dom_html_frame_element_set_scrolling;
h$webkit_dom_html_frame_element_set_scrolling = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["scrolling"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_frame_element_get_scrolling;
h$webkit_dom_html_frame_element_get_scrolling = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["scrolling"]);
                                                 });
var h$webkit_dom_html_frame_element_set_src;
h$webkit_dom_html_frame_element_set_src = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["src"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_frame_element_get_src;
h$webkit_dom_html_frame_element_get_src = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["src"]);
                                           });
var h$webkit_dom_html_frame_element_get_content_document;
h$webkit_dom_html_frame_element_get_content_document = (function(self,
                                                        self_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["contentDocument"];
                                                        });
var h$webkit_dom_html_frame_element_get_content_window;
h$webkit_dom_html_frame_element_get_content_window = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["contentWindow"];
                                                      });
var h$webkit_dom_html_frame_element_get_width;
h$webkit_dom_html_frame_element_get_width = (function(self,
                                             self_2)
                                             {
                                               return self["width"];
                                             });
var h$webkit_dom_html_frame_element_get_height;
h$webkit_dom_html_frame_element_get_height = (function(self,
                                              self_2)
                                              {
                                                return self["height"];
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_form_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLFormElement);
                                           });
var h$webkit_dom_html_form_element_submit;
h$webkit_dom_html_form_element_submit = (function(self,
                                         self_2)
                                         {
                                           return self["submit"]();
                                         });
var h$webkit_dom_html_form_element_reset;
h$webkit_dom_html_form_element_reset = (function(self,
                                        self_2)
                                        {
                                          return self["reset"]();
                                        });
var h$webkit_dom_html_form_element_check_validity;
h$webkit_dom_html_form_element_check_validity = (function(self,
                                                 self_2)
                                                 {
                                                   return self["checkValidity"]();
                                                 });
var h$webkit_dom_html_form_element_set_accept_charset;
h$webkit_dom_html_form_element_set_accept_charset = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["acceptCharset"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_form_element_get_accept_charset;
h$webkit_dom_html_form_element_get_accept_charset = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["acceptCharset"]);
                                                     });
var h$webkit_dom_html_form_element_set_action;
h$webkit_dom_html_form_element_set_action = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["action"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_form_element_get_action;
h$webkit_dom_html_form_element_get_action = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["action"]);
                                             });
var h$webkit_dom_html_form_element_set_autocomplete;
h$webkit_dom_html_form_element_set_autocomplete = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["autocomplete"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_html_form_element_get_autocomplete;
h$webkit_dom_html_form_element_get_autocomplete = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["autocomplete"]);
                                                   });
var h$webkit_dom_html_form_element_set_enctype;
h$webkit_dom_html_form_element_set_enctype = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["enctype"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_form_element_get_enctype;
h$webkit_dom_html_form_element_get_enctype = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["enctype"]);
                                              });
var h$webkit_dom_html_form_element_set_encoding;
h$webkit_dom_html_form_element_set_encoding = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["encoding"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_form_element_get_encoding;
h$webkit_dom_html_form_element_get_encoding = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["encoding"]);
                                               });
var h$webkit_dom_html_form_element_set_method;
h$webkit_dom_html_form_element_set_method = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["method"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_form_element_get_method;
h$webkit_dom_html_form_element_get_method = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["method"]);
                                             });
var h$webkit_dom_html_form_element_set_name;
h$webkit_dom_html_form_element_set_name = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["name"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_form_element_get_name;
h$webkit_dom_html_form_element_get_name = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["name"]);
                                           });
var h$webkit_dom_html_form_element_set_no_validate;
h$webkit_dom_html_form_element_set_no_validate = (function(self,
                                                  self_2, val)
                                                  {
                                                    self["noValidate"] = val;
                                                  });
var h$webkit_dom_html_form_element_get_no_validate;
h$webkit_dom_html_form_element_get_no_validate = (function(self,
                                                  self_2)
                                                  {
                                                    return self["noValidate"];
                                                  });
var h$webkit_dom_html_form_element_set_target;
h$webkit_dom_html_form_element_set_target = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["target"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_form_element_get_target;
h$webkit_dom_html_form_element_get_target = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["target"]);
                                             });
var h$webkit_dom_html_form_element_get_elements;
h$webkit_dom_html_form_element_get_elements = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["elements"];
                                               });
var h$webkit_dom_html_form_element_get_length;
h$webkit_dom_html_form_element_get_length = (function(self,
                                             self_2)
                                             {
                                               return self["length"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_font_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLFontElement);
                                           });
var h$webkit_dom_html_font_element_set_color;
h$webkit_dom_html_font_element_set_color = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["color"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_font_element_get_color;
h$webkit_dom_html_font_element_get_color = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["color"]);
                                            });
var h$webkit_dom_html_font_element_set_face;
h$webkit_dom_html_font_element_set_face = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["face"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_font_element_get_face;
h$webkit_dom_html_font_element_get_face = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["face"]);
                                           });
var h$webkit_dom_html_font_element_set_size;
h$webkit_dom_html_font_element_set_size = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["size"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_font_element_get_size;
h$webkit_dom_html_font_element_get_size = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["size"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_field_set_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLFieldSetElement);
                                                });
var h$webkit_dom_html_field_set_element_check_validity;
h$webkit_dom_html_field_set_element_check_validity = (function(self,
                                                      self_2)
                                                      {
                                                        return self["checkValidity"]();
                                                      });
var h$webkit_dom_html_field_set_element_set_custom_validity;
h$webkit_dom_html_field_set_element_set_custom_validity = (function(self,
                                                           self_2, error,
                                                           error_2)
                                                           {
                                                             return self["setCustomValidity"](h$decodeUtf8z(error,
                                                             error_2));
                                                           });
var h$webkit_dom_html_field_set_element_set_disabled;
h$webkit_dom_html_field_set_element_set_disabled = (function(self,
                                                    self_2, val)
                                                    {
                                                      self["disabled"] = val;
                                                    });
var h$webkit_dom_html_field_set_element_get_disabled;
h$webkit_dom_html_field_set_element_get_disabled = (function(self,
                                                    self_2)
                                                    {
                                                      return self["disabled"];
                                                    });
var h$webkit_dom_html_field_set_element_get_form;
h$webkit_dom_html_field_set_element_get_form = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["form"];
                                                });
var h$webkit_dom_html_field_set_element_set_name;
h$webkit_dom_html_field_set_element_set_name = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["name"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_field_set_element_get_name;
h$webkit_dom_html_field_set_element_get_name = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["name"]);
                                                });
var h$webkit_dom_html_field_set_element_get_elements;
h$webkit_dom_html_field_set_element_get_elements = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["elements"];
                                                    });
var h$webkit_dom_html_field_set_element_get_will_validate;
h$webkit_dom_html_field_set_element_get_will_validate = (function(self,
                                                         self_2)
                                                         {
                                                           return self["willValidate"];
                                                         });
var h$webkit_dom_html_field_set_element_get_validity;
h$webkit_dom_html_field_set_element_get_validity = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["validity"];
                                                    });
var h$webkit_dom_html_field_set_element_get_validation_message;
h$webkit_dom_html_field_set_element_get_validation_message = (function(self,
                                                              self_2)
                                                              {
                                                                h$ret1 = 0;
                                                                return h$encodeUtf8(self["validationMessage"]);
                                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_embed_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLEmbedElement);
                                            });
var h$webkit_dom_html_embed_element_set_align;
h$webkit_dom_html_embed_element_set_align = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["align"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_embed_element_get_align;
h$webkit_dom_html_embed_element_get_align = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["align"]);
                                             });
var h$webkit_dom_html_embed_element_set_height;
h$webkit_dom_html_embed_element_set_height = (function(self,
                                              self_2, val)
                                              {
                                                self["height"] = val;
                                              });
var h$webkit_dom_html_embed_element_get_height;
h$webkit_dom_html_embed_element_get_height = (function(self,
                                              self_2)
                                              {
                                                return self["height"];
                                              });
var h$webkit_dom_html_embed_element_set_name;
h$webkit_dom_html_embed_element_set_name = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["name"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_embed_element_get_name;
h$webkit_dom_html_embed_element_get_name = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["name"]);
                                            });
var h$webkit_dom_html_embed_element_set_src;
h$webkit_dom_html_embed_element_set_src = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["src"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_embed_element_get_src;
h$webkit_dom_html_embed_element_get_src = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["src"]);
                                           });
var h$webkit_dom_html_embed_element_set_width;
h$webkit_dom_html_embed_element_set_width = (function(self,
                                             self_2, val)
                                             {
                                               self["width"] = val;
                                             });
var h$webkit_dom_html_embed_element_get_width;
h$webkit_dom_html_embed_element_get_width = (function(self,
                                             self_2)
                                             {
                                               return self["width"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_element_get_type = (function()
                                      {
                                        return h$g_get_type(HTMLElement);
                                      });
var h$webkit_dom_html_element_insert_adjacent_element;
h$webkit_dom_html_element_insert_adjacent_element = (function(self,
                                                     self_2, where, where_2,
                                                     element, element_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["insertAdjacentElement"](h$decodeUtf8z(where,
                                                       where_2), element);
                                                     });
var h$webkit_dom_html_element_insert_adjacent_html;
h$webkit_dom_html_element_insert_adjacent_html = (function(self,
                                                  self_2, where, where_2, html,
                                                  html_2)
                                                  {
                                                    return self["insertAdjacentHTML"](h$decodeUtf8z(where,
                                                    where_2),
                                                    h$decodeUtf8z(html,
                                                    html_2));
                                                  });
var h$webkit_dom_html_element_insert_adjacent_text;
h$webkit_dom_html_element_insert_adjacent_text = (function(self,
                                                  self_2, where, where_2, text,
                                                  text_2)
                                                  {
                                                    return self["insertAdjacentText"](h$decodeUtf8z(where,
                                                    where_2),
                                                    h$decodeUtf8z(text,
                                                    text_2));
                                                  });
var h$webkit_dom_html_element_click;
h$webkit_dom_html_element_click = (function(self,
                                   self_2)
                                   {
                                     return self["click"]();
                                   });
var h$webkit_dom_html_element_set_id;
h$webkit_dom_html_element_set_id = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["id"] = h$decodeUtf8z(val,
                                      val_2);
                                    });
var h$webkit_dom_html_element_get_id;
h$webkit_dom_html_element_get_id = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["id"]);
                                    });
var h$webkit_dom_html_element_set_title;
h$webkit_dom_html_element_set_title = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["title"] = h$decodeUtf8z(val,
                                         val_2);
                                       });
var h$webkit_dom_html_element_get_title;
h$webkit_dom_html_element_get_title = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["title"]);
                                       });
var h$webkit_dom_html_element_set_lang;
h$webkit_dom_html_element_set_lang = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["lang"] = h$decodeUtf8z(val,
                                        val_2);
                                      });
var h$webkit_dom_html_element_get_lang;
h$webkit_dom_html_element_get_lang = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["lang"]);
                                      });
var h$webkit_dom_html_element_set_translate;
h$webkit_dom_html_element_set_translate = (function(self,
                                           self_2, val)
                                           {
                                             self["translate"] = val;
                                           });
var h$webkit_dom_html_element_get_translate;
h$webkit_dom_html_element_get_translate = (function(self,
                                           self_2)
                                           {
                                             return self["translate"];
                                           });
var h$webkit_dom_html_element_set_dir;
h$webkit_dom_html_element_set_dir = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["dir"] = h$decodeUtf8z(val,
                                       val_2);
                                     });
var h$webkit_dom_html_element_get_dir;
h$webkit_dom_html_element_get_dir = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return h$encodeUtf8(self["dir"]);
                                     });
var h$webkit_dom_html_element_set_tab_index;
h$webkit_dom_html_element_set_tab_index = (function(self,
                                           self_2, val)
                                           {
                                             self["tabIndex"] = val;
                                           });
var h$webkit_dom_html_element_get_tab_index;
h$webkit_dom_html_element_get_tab_index = (function(self,
                                           self_2)
                                           {
                                             return self["tabIndex"];
                                           });
var h$webkit_dom_html_element_set_draggable;
h$webkit_dom_html_element_set_draggable = (function(self,
                                           self_2, val)
                                           {
                                             self["draggable"] = val;
                                           });
var h$webkit_dom_html_element_get_draggable;
h$webkit_dom_html_element_get_draggable = (function(self,
                                           self_2)
                                           {
                                             return self["draggable"];
                                           });
var h$webkit_dom_html_element_set_webkitdropzone;
h$webkit_dom_html_element_set_webkitdropzone = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["webkitdropzone"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_element_get_webkitdropzone;
h$webkit_dom_html_element_get_webkitdropzone = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["webkitdropzone"]);
                                                });
var h$webkit_dom_html_element_set_hidden;
h$webkit_dom_html_element_set_hidden = (function(self,
                                        self_2, val)
                                        {
                                          self["hidden"] = val;
                                        });
var h$webkit_dom_html_element_get_hidden;
h$webkit_dom_html_element_get_hidden = (function(self,
                                        self_2)
                                        {
                                          return self["hidden"];
                                        });
var h$webkit_dom_html_element_set_access_key;
h$webkit_dom_html_element_set_access_key = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["accessKey"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_element_get_access_key;
h$webkit_dom_html_element_get_access_key = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["accessKey"]);
                                            });
var h$webkit_dom_html_element_set_inner_html;
h$webkit_dom_html_element_set_inner_html = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["innerHTML"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_element_get_inner_html;
h$webkit_dom_html_element_get_inner_html = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["innerHTML"]);
                                            });
var h$webkit_dom_html_element_set_inner_text;
h$webkit_dom_html_element_set_inner_text = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["innerText"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_element_get_inner_text;
h$webkit_dom_html_element_get_inner_text = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["innerText"]);
                                            });
var h$webkit_dom_html_element_set_outer_html;
h$webkit_dom_html_element_set_outer_html = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["outerHTML"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_element_get_outer_html;
h$webkit_dom_html_element_get_outer_html = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["outerHTML"]);
                                            });
var h$webkit_dom_html_element_set_outer_text;
h$webkit_dom_html_element_set_outer_text = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["outerText"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_element_get_outer_text;
h$webkit_dom_html_element_get_outer_text = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["outerText"]);
                                            });
var h$webkit_dom_html_element_get_children;
h$webkit_dom_html_element_get_children = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["children"];
                                          });
var h$webkit_dom_html_element_set_content_editable;
h$webkit_dom_html_element_set_content_editable = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["contentEditable"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_element_get_content_editable;
h$webkit_dom_html_element_get_content_editable = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["contentEditable"]);
                                                  });
var h$webkit_dom_html_element_get_is_content_editable;
h$webkit_dom_html_element_get_is_content_editable = (function(self,
                                                     self_2)
                                                     {
                                                       return self["isContentEditable"];
                                                     });
var h$webkit_dom_html_element_set_spellcheck;
h$webkit_dom_html_element_set_spellcheck = (function(self,
                                            self_2, val)
                                            {
                                              self["spellcheck"] = val;
                                            });
var h$webkit_dom_html_element_get_spellcheck;
h$webkit_dom_html_element_get_spellcheck = (function(self,
                                            self_2)
                                            {
                                              return self["spellcheck"];
                                            });
var h$webkit_dom_html_element_set_item_scope;
h$webkit_dom_html_element_set_item_scope = (function(self,
                                            self_2, val)
                                            {
                                              self["itemScope"] = val;
                                            });
var h$webkit_dom_html_element_get_item_scope;
h$webkit_dom_html_element_get_item_scope = (function(self,
                                            self_2)
                                            {
                                              return self["itemScope"];
                                            });
var h$webkit_dom_html_element_get_item_type;
h$webkit_dom_html_element_get_item_type = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["itemType"];
                                           });
var h$webkit_dom_html_element_set_item_id;
h$webkit_dom_html_element_set_item_id = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["itemId"] = h$decodeUtf8z(val,
                                           val_2);
                                         });
var h$webkit_dom_html_element_get_item_id;
h$webkit_dom_html_element_get_item_id = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["itemId"]);
                                         });
var h$webkit_dom_html_element_get_item_ref;
h$webkit_dom_html_element_get_item_ref = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["itemRef"];
                                          });
var h$webkit_dom_html_element_get_item_prop;
h$webkit_dom_html_element_get_item_prop = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["itemProp"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_document_get_type = (function()
                                       {
                                         return h$g_get_type(HTMLDocument);
                                       });
var h$webkit_dom_html_document_open;
h$webkit_dom_html_document_open = (function(self,
                                   self_2)
                                   {
                                     return self["open"]();
                                   });
var h$webkit_dom_html_document_close;
h$webkit_dom_html_document_close = (function(self,
                                    self_2)
                                    {
                                      return self["close"]();
                                    });
var h$webkit_dom_html_document_clear;
h$webkit_dom_html_document_clear = (function(self,
                                    self_2)
                                    {
                                      return self["clear"]();
                                    });
var h$webkit_dom_html_document_capture_events;
h$webkit_dom_html_document_capture_events = (function(self,
                                             self_2)
                                             {
                                               return self["captureEvents"]();
                                             });
var h$webkit_dom_html_document_release_events;
h$webkit_dom_html_document_release_events = (function(self,
                                             self_2)
                                             {
                                               return self["releaseEvents"]();
                                             });
var h$webkit_dom_html_document_has_focus;
h$webkit_dom_html_document_has_focus = (function(self,
                                        self_2)
                                        {
                                          return self["hasFocus"]();
                                        });
var h$webkit_dom_html_document_get_embeds;
h$webkit_dom_html_document_get_embeds = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["embeds"];
                                         });
var h$webkit_dom_html_document_get_plugins;
h$webkit_dom_html_document_get_plugins = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["plugins"];
                                          });
var h$webkit_dom_html_document_get_scripts;
h$webkit_dom_html_document_get_scripts = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["scripts"];
                                          });
var h$webkit_dom_html_document_get_width;
h$webkit_dom_html_document_get_width = (function(self,
                                        self_2)
                                        {
                                          return self["width"];
                                        });
var h$webkit_dom_html_document_get_height;
h$webkit_dom_html_document_get_height = (function(self,
                                         self_2)
                                         {
                                           return self["height"];
                                         });
var h$webkit_dom_html_document_set_dir;
h$webkit_dom_html_document_set_dir = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["dir"] = h$decodeUtf8z(val,
                                        val_2);
                                      });
var h$webkit_dom_html_document_get_dir;
h$webkit_dom_html_document_get_dir = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["dir"]);
                                      });
var h$webkit_dom_html_document_set_design_mode;
h$webkit_dom_html_document_set_design_mode = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["designMode"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_document_get_design_mode;
h$webkit_dom_html_document_get_design_mode = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["designMode"]);
                                              });
var h$webkit_dom_html_document_get_compat_mode;
h$webkit_dom_html_document_get_compat_mode = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["compatMode"]);
                                              });
var h$webkit_dom_html_document_get_active_element;
h$webkit_dom_html_document_get_active_element = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["activeElement"];
                                                 });
var h$webkit_dom_html_document_set_bg_color;
h$webkit_dom_html_document_set_bg_color = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["bgColor"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_document_get_bg_color;
h$webkit_dom_html_document_get_bg_color = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["bgColor"]);
                                           });
var h$webkit_dom_html_document_set_fg_color;
h$webkit_dom_html_document_set_fg_color = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["fgColor"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_document_get_fg_color;
h$webkit_dom_html_document_get_fg_color = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["fgColor"]);
                                           });
var h$webkit_dom_html_document_set_alink_color;
h$webkit_dom_html_document_set_alink_color = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["alinkColor"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_document_get_alink_color;
h$webkit_dom_html_document_get_alink_color = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["alinkColor"]);
                                              });
var h$webkit_dom_html_document_set_link_color;
h$webkit_dom_html_document_set_link_color = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["linkColor"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_document_get_link_color;
h$webkit_dom_html_document_get_link_color = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["linkColor"]);
                                             });
var h$webkit_dom_html_document_set_vlink_color;
h$webkit_dom_html_document_set_vlink_color = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["vlinkColor"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_document_get_vlink_color;
h$webkit_dom_html_document_get_vlink_color = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["vlinkColor"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_htmld_list_element_get_type = (function()
                                            {
                                              return h$g_get_type(HTMLDListElement);
                                            });
var h$webkit_dom_htmld_list_element_set_compact;
h$webkit_dom_htmld_list_element_set_compact = (function(self,
                                               self_2, val)
                                               {
                                                 self["compact"] = val;
                                               });
var h$webkit_dom_htmld_list_element_get_compact;
h$webkit_dom_htmld_list_element_get_compact = (function(self,
                                               self_2)
                                               {
                                                 return self["compact"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_div_element_get_type = (function()
                                          {
                                            return h$g_get_type(HTMLDivElement);
                                          });
var h$webkit_dom_html_div_element_set_align;
h$webkit_dom_html_div_element_set_align = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["align"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_div_element_get_align;
h$webkit_dom_html_div_element_get_align = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["align"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_directory_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLDirectoryElement);
                                                });
var h$webkit_dom_html_directory_element_set_compact;
h$webkit_dom_html_directory_element_set_compact = (function(self,
                                                   self_2, val)
                                                   {
                                                     self["compact"] = val;
                                                   });
var h$webkit_dom_html_directory_element_get_compact;
h$webkit_dom_html_directory_element_get_compact = (function(self,
                                                   self_2)
                                                   {
                                                     return self["compact"];
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_details_element_get_type = (function()
                                              {
                                                return h$g_get_type(HTMLDetailsElement);
                                              });
var h$webkit_dom_html_details_element_set_open;
h$webkit_dom_html_details_element_set_open = (function(self,
                                              self_2, val)
                                              {
                                                self["open"] = val;
                                              });
var h$webkit_dom_html_details_element_get_open;
h$webkit_dom_html_details_element_get_open = (function(self,
                                              self_2)
                                              {
                                                return self["open"];
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_collection_get_type = (function()
                                         {
                                           return h$g_get_type(HTMLCollection);
                                         });
var h$webkit_dom_html_collection_item;
h$webkit_dom_html_collection_item = (function(self,
                                     self_2, index)
                                     {
                                       h$ret1 = 0;
                                       return self["item"](index);
                                     });
var h$webkit_dom_html_collection_get_length;
h$webkit_dom_html_collection_get_length = (function(self,
                                           self_2)
                                           {
                                             return self["length"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_canvas_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLCanvasElement);
                                             });
var h$webkit_dom_html_canvas_element_set_width;
h$webkit_dom_html_canvas_element_set_width = (function(self,
                                              self_2, val)
                                              {
                                                self["width"] = val;
                                              });
var h$webkit_dom_html_canvas_element_get_width;
h$webkit_dom_html_canvas_element_get_width = (function(self,
                                              self_2)
                                              {
                                                return self["width"];
                                              });
var h$webkit_dom_html_canvas_element_set_height;
h$webkit_dom_html_canvas_element_set_height = (function(self,
                                               self_2, val)
                                               {
                                                 self["height"] = val;
                                               });
var h$webkit_dom_html_canvas_element_get_height;
h$webkit_dom_html_canvas_element_get_height = (function(self,
                                               self_2)
                                               {
                                                 return self["height"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_button_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLButtonElement);
                                             });
var h$webkit_dom_html_button_element_check_validity;
h$webkit_dom_html_button_element_check_validity = (function(self,
                                                   self_2)
                                                   {
                                                     return self["checkValidity"]();
                                                   });
var h$webkit_dom_html_button_element_set_custom_validity;
h$webkit_dom_html_button_element_set_custom_validity = (function(self,
                                                        self_2, error, error_2)
                                                        {
                                                          return self["setCustomValidity"](h$decodeUtf8z(error,
                                                          error_2));
                                                        });
var h$webkit_dom_html_button_element_set_autofocus;
h$webkit_dom_html_button_element_set_autofocus = (function(self,
                                                  self_2, val)
                                                  {
                                                    self["autofocus"] = val;
                                                  });
var h$webkit_dom_html_button_element_get_autofocus;
h$webkit_dom_html_button_element_get_autofocus = (function(self,
                                                  self_2)
                                                  {
                                                    return self["autofocus"];
                                                  });
var h$webkit_dom_html_button_element_set_disabled;
h$webkit_dom_html_button_element_set_disabled = (function(self,
                                                 self_2, val)
                                                 {
                                                   self["disabled"] = val;
                                                 });
var h$webkit_dom_html_button_element_get_disabled;
h$webkit_dom_html_button_element_get_disabled = (function(self,
                                                 self_2)
                                                 {
                                                   return self["disabled"];
                                                 });
var h$webkit_dom_html_button_element_get_form;
h$webkit_dom_html_button_element_get_form = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["form"];
                                             });
var h$webkit_dom_html_button_element_set_form_action;
h$webkit_dom_html_button_element_set_form_action = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["formAction"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_button_element_get_form_action;
h$webkit_dom_html_button_element_get_form_action = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["formAction"]);
                                                    });
var h$webkit_dom_html_button_element_set_form_enctype;
h$webkit_dom_html_button_element_set_form_enctype = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["formEnctype"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_html_button_element_get_form_enctype;
h$webkit_dom_html_button_element_get_form_enctype = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["formEnctype"]);
                                                     });
var h$webkit_dom_html_button_element_set_form_method;
h$webkit_dom_html_button_element_set_form_method = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["formMethod"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_button_element_get_form_method;
h$webkit_dom_html_button_element_get_form_method = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["formMethod"]);
                                                    });
var h$webkit_dom_html_button_element_set_form_no_validate;
h$webkit_dom_html_button_element_set_form_no_validate = (function(self,
                                                         self_2, val)
                                                         {
                                                           self["formNoValidate"] = val;
                                                         });
var h$webkit_dom_html_button_element_get_form_no_validate;
h$webkit_dom_html_button_element_get_form_no_validate = (function(self,
                                                         self_2)
                                                         {
                                                           return self["formNoValidate"];
                                                         });
var h$webkit_dom_html_button_element_set_form_target;
h$webkit_dom_html_button_element_set_form_target = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["formTarget"] = h$decodeUtf8z(val,
                                                      val_2);
                                                    });
var h$webkit_dom_html_button_element_get_form_target;
h$webkit_dom_html_button_element_get_form_target = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return h$encodeUtf8(self["formTarget"]);
                                                    });
var h$webkit_dom_html_button_element_set_name;
h$webkit_dom_html_button_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_button_element_get_name;
h$webkit_dom_html_button_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_button_element_set_value;
h$webkit_dom_html_button_element_set_value = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["value"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_button_element_get_value;
h$webkit_dom_html_button_element_get_value = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["value"]);
                                              });
var h$webkit_dom_html_button_element_get_will_validate;
h$webkit_dom_html_button_element_get_will_validate = (function(self,
                                                      self_2)
                                                      {
                                                        return self["willValidate"];
                                                      });
var h$webkit_dom_html_button_element_get_validity;
h$webkit_dom_html_button_element_get_validity = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["validity"];
                                                 });
var h$webkit_dom_html_button_element_get_validation_message;
h$webkit_dom_html_button_element_get_validation_message = (function(self,
                                                           self_2)
                                                           {
                                                             h$ret1 = 0;
                                                             return h$encodeUtf8(self["validationMessage"]);
                                                           });
var h$webkit_dom_html_button_element_get_labels;
h$webkit_dom_html_button_element_get_labels = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["labels"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_htmlbr_element_get_type = (function()
                                        {
                                          return h$g_get_type(HTMLBRElement);
                                        });
var h$webkit_dom_htmlbr_element_set_clear;
h$webkit_dom_htmlbr_element_set_clear = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["clear"] = h$decodeUtf8z(val,
                                           val_2);
                                         });
var h$webkit_dom_htmlbr_element_get_clear;
h$webkit_dom_htmlbr_element_get_clear = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["clear"]);
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_body_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLBodyElement);
                                           });
var h$webkit_dom_html_body_element_set_a_link;
h$webkit_dom_html_body_element_set_a_link = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["aLink"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_body_element_get_a_link;
h$webkit_dom_html_body_element_get_a_link = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["aLink"]);
                                             });
var h$webkit_dom_html_body_element_set_background;
h$webkit_dom_html_body_element_set_background = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["background"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_body_element_get_background;
h$webkit_dom_html_body_element_get_background = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["background"]);
                                                 });
var h$webkit_dom_html_body_element_set_bg_color;
h$webkit_dom_html_body_element_set_bg_color = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["bgColor"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_body_element_get_bg_color;
h$webkit_dom_html_body_element_get_bg_color = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["bgColor"]);
                                               });
var h$webkit_dom_html_body_element_set_link;
h$webkit_dom_html_body_element_set_link = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["link"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_body_element_get_link;
h$webkit_dom_html_body_element_get_link = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["link"]);
                                           });
var h$webkit_dom_html_body_element_set_text;
h$webkit_dom_html_body_element_set_text = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["text"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_body_element_get_text;
h$webkit_dom_html_body_element_get_text = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["text"]);
                                           });
var h$webkit_dom_html_body_element_set_v_link;
h$webkit_dom_html_body_element_set_v_link = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["vLink"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_body_element_get_v_link;
h$webkit_dom_html_body_element_get_v_link = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["vLink"]);
                                             });
var h$webkit_dom_html_body_element_set_onbeforeunload;
h$webkit_dom_html_body_element_set_onbeforeunload = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onbeforeunload"] = val;
                                                     });
var h$webkit_dom_html_body_element_get_onbeforeunload;
h$webkit_dom_html_body_element_get_onbeforeunload = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onbeforeunload"];
                                                     });
var h$webkit_dom_html_body_element_set_onhashchange;
h$webkit_dom_html_body_element_set_onhashchange = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["onhashchange"] = val;
                                                   });
var h$webkit_dom_html_body_element_get_onhashchange;
h$webkit_dom_html_body_element_get_onhashchange = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["onhashchange"];
                                                   });
var h$webkit_dom_html_body_element_set_onmessage;
h$webkit_dom_html_body_element_set_onmessage = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["onmessage"] = val;
                                                });
var h$webkit_dom_html_body_element_get_onmessage;
h$webkit_dom_html_body_element_get_onmessage = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["onmessage"];
                                                });
var h$webkit_dom_html_body_element_set_onoffline;
h$webkit_dom_html_body_element_set_onoffline = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["onoffline"] = val;
                                                });
var h$webkit_dom_html_body_element_get_onoffline;
h$webkit_dom_html_body_element_get_onoffline = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["onoffline"];
                                                });
var h$webkit_dom_html_body_element_set_ononline;
h$webkit_dom_html_body_element_set_ononline = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["ononline"] = val;
                                               });
var h$webkit_dom_html_body_element_get_ononline;
h$webkit_dom_html_body_element_get_ononline = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["ononline"];
                                               });
var h$webkit_dom_html_body_element_set_onpopstate;
h$webkit_dom_html_body_element_set_onpopstate = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["onpopstate"] = val;
                                                 });
var h$webkit_dom_html_body_element_get_onpopstate;
h$webkit_dom_html_body_element_get_onpopstate = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["onpopstate"];
                                                 });
var h$webkit_dom_html_body_element_set_onresize;
h$webkit_dom_html_body_element_set_onresize = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["onresize"] = val;
                                               });
var h$webkit_dom_html_body_element_get_onresize;
h$webkit_dom_html_body_element_get_onresize = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["onresize"];
                                               });
var h$webkit_dom_html_body_element_set_onstorage;
h$webkit_dom_html_body_element_set_onstorage = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["onstorage"] = val;
                                                });
var h$webkit_dom_html_body_element_get_onstorage;
h$webkit_dom_html_body_element_get_onstorage = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["onstorage"];
                                                });
var h$webkit_dom_html_body_element_set_onunload;
h$webkit_dom_html_body_element_set_onunload = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["onunload"] = val;
                                               });
var h$webkit_dom_html_body_element_get_onunload;
h$webkit_dom_html_body_element_get_onunload = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["onunload"];
                                               });
var h$webkit_dom_html_body_element_set_onorientationchange;
h$webkit_dom_html_body_element_set_onorientationchange = (function(self,
                                                          self_2, val, val_2)
                                                          {
                                                            self["onorientationchange"] = val;
                                                          });
var h$webkit_dom_html_body_element_get_onorientationchange;
h$webkit_dom_html_body_element_get_onorientationchange = (function(self,
                                                          self_2)
                                                          {
                                                            h$ret1 = 0;
                                                            return self["onorientationchange"];
                                                          });
var h$webkit_dom_html_body_element_set_onblur;
h$webkit_dom_html_body_element_set_onblur = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["onblur"] = val;
                                             });
var h$webkit_dom_html_body_element_get_onblur;
h$webkit_dom_html_body_element_get_onblur = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["onblur"];
                                             });
var h$webkit_dom_html_body_element_set_onerror;
h$webkit_dom_html_body_element_set_onerror = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["onerror"] = val;
                                              });
var h$webkit_dom_html_body_element_get_onerror;
h$webkit_dom_html_body_element_get_onerror = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["onerror"];
                                              });
var h$webkit_dom_html_body_element_set_onfocus;
h$webkit_dom_html_body_element_set_onfocus = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["onfocus"] = val;
                                              });
var h$webkit_dom_html_body_element_get_onfocus;
h$webkit_dom_html_body_element_get_onfocus = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["onfocus"];
                                              });
var h$webkit_dom_html_body_element_set_onload;
h$webkit_dom_html_body_element_set_onload = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["onload"] = val;
                                             });
var h$webkit_dom_html_body_element_get_onload;
h$webkit_dom_html_body_element_get_onload = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["onload"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_base_font_element_get_type = (function()
                                                {
                                                  return h$g_get_type(HTMLBaseFontElement);
                                                });
var h$webkit_dom_html_base_font_element_set_color;
h$webkit_dom_html_base_font_element_set_color = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["color"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_base_font_element_get_color;
h$webkit_dom_html_base_font_element_get_color = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["color"]);
                                                 });
var h$webkit_dom_html_base_font_element_set_face;
h$webkit_dom_html_base_font_element_set_face = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["face"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_base_font_element_get_face;
h$webkit_dom_html_base_font_element_get_face = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["face"]);
                                                });
var h$webkit_dom_html_base_font_element_set_size;
h$webkit_dom_html_base_font_element_set_size = (function(self,
                                                self_2, val)
                                                {
                                                  self["size"] = val;
                                                });
var h$webkit_dom_html_base_font_element_get_size;
h$webkit_dom_html_base_font_element_get_size = (function(self,
                                                self_2)
                                                {
                                                  return self["size"];
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_base_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLBaseElement);
                                           });
var h$webkit_dom_html_base_element_set_href;
h$webkit_dom_html_base_element_set_href = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["href"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_base_element_get_href;
h$webkit_dom_html_base_element_get_href = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["href"]);
                                           });
var h$webkit_dom_html_base_element_set_target;
h$webkit_dom_html_base_element_set_target = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["target"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_base_element_get_target;
h$webkit_dom_html_base_element_get_target = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["target"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_area_element_get_type = (function()
                                           {
                                             return h$g_get_type(HTMLAreaElement);
                                           });
var h$webkit_dom_html_area_element_set_alt;
h$webkit_dom_html_area_element_set_alt = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["alt"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_html_area_element_get_alt;
h$webkit_dom_html_area_element_get_alt = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["alt"]);
                                          });
var h$webkit_dom_html_area_element_set_coords;
h$webkit_dom_html_area_element_set_coords = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["coords"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_area_element_get_coords;
h$webkit_dom_html_area_element_get_coords = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["coords"]);
                                             });
var h$webkit_dom_html_area_element_set_href;
h$webkit_dom_html_area_element_set_href = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["href"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_area_element_get_href;
h$webkit_dom_html_area_element_get_href = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["href"]);
                                           });
var h$webkit_dom_html_area_element_set_no_href;
h$webkit_dom_html_area_element_set_no_href = (function(self,
                                              self_2, val)
                                              {
                                                self["noHref"] = val;
                                              });
var h$webkit_dom_html_area_element_get_no_href;
h$webkit_dom_html_area_element_get_no_href = (function(self,
                                              self_2)
                                              {
                                                return self["noHref"];
                                              });
var h$webkit_dom_html_area_element_set_ping;
h$webkit_dom_html_area_element_set_ping = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["ping"] = h$decodeUtf8z(val,
                                             val_2);
                                           });
var h$webkit_dom_html_area_element_get_ping;
h$webkit_dom_html_area_element_get_ping = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["ping"]);
                                           });
var h$webkit_dom_html_area_element_set_shape;
h$webkit_dom_html_area_element_set_shape = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["shape"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_area_element_get_shape;
h$webkit_dom_html_area_element_get_shape = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["shape"]);
                                            });
var h$webkit_dom_html_area_element_set_target;
h$webkit_dom_html_area_element_set_target = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["target"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_area_element_get_target;
h$webkit_dom_html_area_element_get_target = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["target"]);
                                             });
var h$webkit_dom_html_area_element_get_hash;
h$webkit_dom_html_area_element_get_hash = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["hash"]);
                                           });
var h$webkit_dom_html_area_element_get_host;
h$webkit_dom_html_area_element_get_host = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["host"]);
                                           });
var h$webkit_dom_html_area_element_get_hostname;
h$webkit_dom_html_area_element_get_hostname = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["hostname"]);
                                               });
var h$webkit_dom_html_area_element_get_pathname;
h$webkit_dom_html_area_element_get_pathname = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["pathname"]);
                                               });
var h$webkit_dom_html_area_element_get_port;
h$webkit_dom_html_area_element_get_port = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["port"]);
                                           });
var h$webkit_dom_html_area_element_get_protocol;
h$webkit_dom_html_area_element_get_protocol = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["protocol"]);
                                               });
var h$webkit_dom_html_area_element_get_search;
h$webkit_dom_html_area_element_get_search = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["search"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_applet_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLAppletElement);
                                             });
var h$webkit_dom_html_applet_element_set_align;
h$webkit_dom_html_applet_element_set_align = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["align"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_applet_element_get_align;
h$webkit_dom_html_applet_element_get_align = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["align"]);
                                              });
var h$webkit_dom_html_applet_element_set_alt;
h$webkit_dom_html_applet_element_set_alt = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["alt"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_applet_element_get_alt;
h$webkit_dom_html_applet_element_get_alt = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["alt"]);
                                            });
var h$webkit_dom_html_applet_element_set_archive;
h$webkit_dom_html_applet_element_set_archive = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["archive"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_applet_element_get_archive;
h$webkit_dom_html_applet_element_get_archive = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["archive"]);
                                                });
var h$webkit_dom_html_applet_element_set_code;
h$webkit_dom_html_applet_element_set_code = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["code"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_applet_element_get_code;
h$webkit_dom_html_applet_element_get_code = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["code"]);
                                             });
var h$webkit_dom_html_applet_element_set_code_base;
h$webkit_dom_html_applet_element_set_code_base = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["codeBase"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_html_applet_element_get_code_base;
h$webkit_dom_html_applet_element_get_code_base = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["codeBase"]);
                                                  });
var h$webkit_dom_html_applet_element_set_height;
h$webkit_dom_html_applet_element_set_height = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["height"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_applet_element_get_height;
h$webkit_dom_html_applet_element_get_height = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["height"]);
                                               });
var h$webkit_dom_html_applet_element_set_hspace;
h$webkit_dom_html_applet_element_set_hspace = (function(self,
                                               self_2, val)
                                               {
                                                 self["hspace"] = val;
                                               });
var h$webkit_dom_html_applet_element_get_hspace;
h$webkit_dom_html_applet_element_get_hspace = (function(self,
                                               self_2)
                                               {
                                                 return self["hspace"];
                                               });
var h$webkit_dom_html_applet_element_set_name;
h$webkit_dom_html_applet_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_applet_element_get_name;
h$webkit_dom_html_applet_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_applet_element_set_object;
h$webkit_dom_html_applet_element_set_object = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["object"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_applet_element_get_object;
h$webkit_dom_html_applet_element_get_object = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["object"]);
                                               });
var h$webkit_dom_html_applet_element_set_vspace;
h$webkit_dom_html_applet_element_set_vspace = (function(self,
                                               self_2, val)
                                               {
                                                 self["vspace"] = val;
                                               });
var h$webkit_dom_html_applet_element_get_vspace;
h$webkit_dom_html_applet_element_get_vspace = (function(self,
                                               self_2)
                                               {
                                                 return self["vspace"];
                                               });
var h$webkit_dom_html_applet_element_set_width;
h$webkit_dom_html_applet_element_set_width = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["width"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_applet_element_get_width;
h$webkit_dom_html_applet_element_get_width = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["width"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_html_anchor_element_get_type = (function()
                                             {
                                               return h$g_get_type(HTMLAnchorElement);
                                             });
var h$webkit_dom_html_anchor_element_set_charset;
h$webkit_dom_html_anchor_element_set_charset = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["charset"] = h$decodeUtf8z(val,
                                                  val_2);
                                                });
var h$webkit_dom_html_anchor_element_get_charset;
h$webkit_dom_html_anchor_element_get_charset = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return h$encodeUtf8(self["charset"]);
                                                });
var h$webkit_dom_html_anchor_element_set_coords;
h$webkit_dom_html_anchor_element_set_coords = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["coords"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_anchor_element_get_coords;
h$webkit_dom_html_anchor_element_get_coords = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["coords"]);
                                               });
var h$webkit_dom_html_anchor_element_set_download;
h$webkit_dom_html_anchor_element_set_download = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["download"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_anchor_element_get_download;
h$webkit_dom_html_anchor_element_get_download = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["download"]);
                                                 });
var h$webkit_dom_html_anchor_element_set_href;
h$webkit_dom_html_anchor_element_set_href = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["href"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_anchor_element_get_href;
h$webkit_dom_html_anchor_element_get_href = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["href"]);
                                             });
var h$webkit_dom_html_anchor_element_set_hreflang;
h$webkit_dom_html_anchor_element_set_hreflang = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["hreflang"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_anchor_element_get_hreflang;
h$webkit_dom_html_anchor_element_get_hreflang = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["hreflang"]);
                                                 });
var h$webkit_dom_html_anchor_element_set_name;
h$webkit_dom_html_anchor_element_set_name = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["name"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_anchor_element_get_name;
h$webkit_dom_html_anchor_element_get_name = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["name"]);
                                             });
var h$webkit_dom_html_anchor_element_set_ping;
h$webkit_dom_html_anchor_element_set_ping = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["ping"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_anchor_element_get_ping;
h$webkit_dom_html_anchor_element_get_ping = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["ping"]);
                                             });
var h$webkit_dom_html_anchor_element_set_rel;
h$webkit_dom_html_anchor_element_set_rel = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["rel"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_anchor_element_get_rel;
h$webkit_dom_html_anchor_element_get_rel = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["rel"]);
                                            });
var h$webkit_dom_html_anchor_element_set_rev;
h$webkit_dom_html_anchor_element_set_rev = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["rev"] = h$decodeUtf8z(val,
                                              val_2);
                                            });
var h$webkit_dom_html_anchor_element_get_rev;
h$webkit_dom_html_anchor_element_get_rev = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["rev"]);
                                            });
var h$webkit_dom_html_anchor_element_set_shape;
h$webkit_dom_html_anchor_element_set_shape = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["shape"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_html_anchor_element_get_shape;
h$webkit_dom_html_anchor_element_get_shape = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["shape"]);
                                              });
var h$webkit_dom_html_anchor_element_set_target;
h$webkit_dom_html_anchor_element_set_target = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["target"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_anchor_element_get_target;
h$webkit_dom_html_anchor_element_get_target = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["target"]);
                                               });
var h$webkit_dom_html_anchor_element_set_hash;
h$webkit_dom_html_anchor_element_set_hash = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["hash"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_anchor_element_get_hash;
h$webkit_dom_html_anchor_element_get_hash = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["hash"]);
                                             });
var h$webkit_dom_html_anchor_element_set_host;
h$webkit_dom_html_anchor_element_set_host = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["host"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_anchor_element_get_host;
h$webkit_dom_html_anchor_element_get_host = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["host"]);
                                             });
var h$webkit_dom_html_anchor_element_set_hostname;
h$webkit_dom_html_anchor_element_set_hostname = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["hostname"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_anchor_element_get_hostname;
h$webkit_dom_html_anchor_element_get_hostname = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["hostname"]);
                                                 });
var h$webkit_dom_html_anchor_element_set_pathname;
h$webkit_dom_html_anchor_element_set_pathname = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["pathname"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_anchor_element_get_pathname;
h$webkit_dom_html_anchor_element_get_pathname = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["pathname"]);
                                                 });
var h$webkit_dom_html_anchor_element_set_port;
h$webkit_dom_html_anchor_element_set_port = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["port"] = h$decodeUtf8z(val,
                                               val_2);
                                             });
var h$webkit_dom_html_anchor_element_get_port;
h$webkit_dom_html_anchor_element_get_port = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["port"]);
                                             });
var h$webkit_dom_html_anchor_element_set_protocol;
h$webkit_dom_html_anchor_element_set_protocol = (function(self,
                                                 self_2, val, val_2)
                                                 {
                                                   self["protocol"] = h$decodeUtf8z(val,
                                                   val_2);
                                                 });
var h$webkit_dom_html_anchor_element_get_protocol;
h$webkit_dom_html_anchor_element_get_protocol = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return h$encodeUtf8(self["protocol"]);
                                                 });
var h$webkit_dom_html_anchor_element_set_search;
h$webkit_dom_html_anchor_element_set_search = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["search"] = h$decodeUtf8z(val,
                                                 val_2);
                                               });
var h$webkit_dom_html_anchor_element_get_search;
h$webkit_dom_html_anchor_element_get_search = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["search"]);
                                               });
var h$webkit_dom_html_anchor_element_get_origin;
h$webkit_dom_html_anchor_element_get_origin = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return h$encodeUtf8(self["origin"]);
                                               });
var h$webkit_dom_html_anchor_element_get_text;
h$webkit_dom_html_anchor_element_get_text = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["text"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_history_get_type = (function()
                                 {
                                   return h$g_get_type(History);
                                 });
var h$webkit_dom_history_get_length;
h$webkit_dom_history_get_length = (function(self,
                                   self_2)
                                   {
                                     return self["length"];
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_geolocation_get_type = (function()
                                     {
                                       return h$g_get_type(Geolocation);
                                     });
var h$webkit_dom_geolocation_clear_watch;
h$webkit_dom_geolocation_clear_watch = (function(self,
                                        self_2, watchId)
                                        {
                                          return self["clearWatch"](watchId);
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_file_list_get_type = (function()
                                   {
                                     return h$g_get_type(FileList);
                                   });
var h$webkit_dom_file_list_item;
h$webkit_dom_file_list_item = (function(self,
                               self_2, index)
                               {
                                 h$ret1 = 0;
                                 return self["item"](index);
                               });
var h$webkit_dom_file_list_get_length;
h$webkit_dom_file_list_get_length = (function(self,
                                     self_2)
                                     {
                                       return self["length"];
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_file_get_type = (function()
                              {
                                return h$g_get_type(File);
                              });
var h$webkit_dom_file_get_name;
h$webkit_dom_file_get_name = (function(self,
                              self_2)
                              {
                                h$ret1 = 0;
                                return h$encodeUtf8(self["name"]);
                              });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_event_target_get_type = (function()
                                      {
                                        return h$g_get_type(EventTarget);
                                      });
var h$webkit_dom_event_target_dispatch_event;
h$webkit_dom_event_target_dispatch_event = (function(self,
                                            self_2, event, event_2)
                                            {
                                              return self["dispatchEvent"](event);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Events
h$webkit_dom_event_get_type = (function()
                               {
                                 return h$g_get_type(Event);
                               });
var h$webkit_dom_event_stop_propagation;
h$webkit_dom_event_stop_propagation = (function(self,
                                       self_2)
                                       {
                                         return self["stopPropagation"]();
                                       });
var h$webkit_dom_event_prevent_default;
h$webkit_dom_event_prevent_default = (function(self,
                                      self_2)
                                      {
                                        return self["preventDefault"]();
                                      });
var h$webkit_dom_event_init_event;
h$webkit_dom_event_init_event = (function(self,
                                 self_2, eventTypeArg,
                                 eventTypeArg_2, canBubbleArg,
                                 cancelableArg)
                                 {
                                   return self["initEvent"](h$decodeUtf8z(eventTypeArg,
                                   eventTypeArg_2), canBubbleArg,
                                   cancelableArg);
                                 });
var h$webkit_dom_event_stop_immediate_propagation;
h$webkit_dom_event_stop_immediate_propagation = (function(self,
                                                 self_2)
                                                 {
                                                   return self["stopImmediatePropagation"]();
                                                 });
var h$webkit_dom_event_get_target;
h$webkit_dom_event_get_target = (function(self,
                                 self_2)
                                 {
                                   h$ret1 = 0;
                                   return self["target"];
                                 });
var h$webkit_dom_event_get_current_target;
h$webkit_dom_event_get_current_target = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["currentTarget"];
                                         });
var h$webkit_dom_event_get_event_phase;
h$webkit_dom_event_get_event_phase = (function(self,
                                      self_2)
                                      {
                                        return self["eventPhase"];
                                      });
var h$webkit_dom_event_get_bubbles;
h$webkit_dom_event_get_bubbles = (function(self,
                                  self_2)
                                  {
                                    return self["bubbles"];
                                  });
var h$webkit_dom_event_get_cancelable;
h$webkit_dom_event_get_cancelable = (function(self,
                                     self_2)
                                     {
                                       return self["cancelable"];
                                     });
var h$webkit_dom_event_get_time_stamp;
h$webkit_dom_event_get_time_stamp = (function(self,
                                     self_2)
                                     {
                                       return self["timeStamp"];
                                     });
var h$webkit_dom_event_get_default_prevented;
h$webkit_dom_event_get_default_prevented = (function(self,
                                            self_2)
                                            {
                                              return self["defaultPrevented"];
                                            });
var h$webkit_dom_event_get_src_element;
h$webkit_dom_event_get_src_element = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["srcElement"];
                                      });
var h$webkit_dom_event_set_return_value;
h$webkit_dom_event_set_return_value = (function(self,
                                       self_2, val)
                                       {
                                         self["returnValue"] = val;
                                       });
var h$webkit_dom_event_get_return_value;
h$webkit_dom_event_get_return_value = (function(self,
                                       self_2)
                                       {
                                         return self["returnValue"];
                                       });
var h$webkit_dom_event_set_cancel_bubble;
h$webkit_dom_event_set_cancel_bubble = (function(self,
                                        self_2, val)
                                        {
                                          self["cancelBubble"] = val;
                                        });
var h$webkit_dom_event_get_cancel_bubble;
h$webkit_dom_event_get_cancel_bubble = (function(self,
                                        self_2)
                                        {
                                          return self["cancelBubble"];
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_entity_reference_get_type = (function()
                                          {
                                            return h$g_get_type(EntityReference);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_element_get_type = (function()
                                 {
                                   return h$g_get_type(Element);
                                 });
var h$webkit_dom_element_get_attribute;
h$webkit_dom_element_get_attribute = (function(self,
                                      self_2, name, name_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["getAttribute"](h$decodeUtf8z(name,
                                        name_2)));
                                      });
var h$webkit_dom_element_set_attribute;
h$webkit_dom_element_set_attribute = (function(self,
                                      self_2, name, name_2, value,
                                      value_2)
                                      {
                                        return self["setAttribute"](h$decodeUtf8z(name,
                                        name_2), h$decodeUtf8z(value,
                                        value_2));
                                      });
var h$webkit_dom_element_remove_attribute;
h$webkit_dom_element_remove_attribute = (function(self,
                                         self_2, name, name_2)
                                         {
                                           return self["removeAttribute"](h$decodeUtf8z(name,
                                           name_2));
                                         });
var h$webkit_dom_element_get_attribute_node;
h$webkit_dom_element_get_attribute_node = (function(self,
                                           self_2, name, name_2)
                                           {
                                             h$ret1 = 0;
                                             return self["getAttributeNode"](h$decodeUtf8z(name,
                                             name_2));
                                           });
var h$webkit_dom_element_set_attribute_node;
h$webkit_dom_element_set_attribute_node = (function(self,
                                           self_2, newAttr, newAttr_2)
                                           {
                                             h$ret1 = 0;
                                             return self["setAttributeNode"](newAttr);
                                           });
var h$webkit_dom_element_remove_attribute_node;
h$webkit_dom_element_remove_attribute_node = (function(self,
                                              self_2, oldAttr, oldAttr_2)
                                              {
                                                h$ret1 = 0;
                                                return self["removeAttributeNode"](oldAttr);
                                              });
var h$webkit_dom_element_get_elements_by_tag_name;
h$webkit_dom_element_get_elements_by_tag_name = (function(self,
                                                 self_2, name, name_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["getElementsByTagName"](h$decodeUtf8z(name,
                                                   name_2));
                                                 });
var h$webkit_dom_element_get_attribute_ns;
h$webkit_dom_element_get_attribute_ns = (function(self,
                                         self_2, namespaceURI,
                                         namespaceURI_2, localName,
                                         localName_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["getAttributeNS"](h$decodeUtf8z(namespaceURI,
                                           namespaceURI_2),
                                           h$decodeUtf8z(localName,
                                           localName_2)));
                                         });
var h$webkit_dom_element_set_attribute_ns;
h$webkit_dom_element_set_attribute_ns = (function(self,
                                         self_2, namespaceURI,
                                         namespaceURI_2, qualifiedName,
                                         qualifiedName_2, value, value_2)
                                         {
                                           return self["setAttributeNS"](h$decodeUtf8z(namespaceURI,
                                           namespaceURI_2),
                                           h$decodeUtf8z(qualifiedName,
                                           qualifiedName_2),
                                           h$decodeUtf8z(value, value_2));
                                         });
var h$webkit_dom_element_remove_attribute_ns;
h$webkit_dom_element_remove_attribute_ns = (function(self,
                                            self_2, namespaceURI,
                                            namespaceURI_2, localName,
                                            localName_2)
                                            {
                                              return self["removeAttributeNS"](h$decodeUtf8z(namespaceURI,
                                              namespaceURI_2),
                                              h$decodeUtf8z(localName,
                                              localName_2));
                                            });
var h$webkit_dom_element_get_elements_by_tag_name_ns;
h$webkit_dom_element_get_elements_by_tag_name_ns = (function(self,
                                                    self_2, namespaceURI,
                                                    namespaceURI_2, localName,
                                                    localName_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["getElementsByTagNameNS"](h$decodeUtf8z(namespaceURI,
                                                      namespaceURI_2),
                                                      h$decodeUtf8z(localName,
                                                      localName_2));
                                                    });
var h$webkit_dom_element_get_attribute_node_ns;
h$webkit_dom_element_get_attribute_node_ns = (function(self,
                                              self_2, namespaceURI,
                                              namespaceURI_2, localName,
                                              localName_2)
                                              {
                                                h$ret1 = 0;
                                                return self["getAttributeNodeNS"](h$decodeUtf8z(namespaceURI,
                                                namespaceURI_2),
                                                h$decodeUtf8z(localName,
                                                localName_2));
                                              });
var h$webkit_dom_element_set_attribute_node_ns;
h$webkit_dom_element_set_attribute_node_ns = (function(self,
                                              self_2, newAttr, newAttr_2)
                                              {
                                                h$ret1 = 0;
                                                return self["setAttributeNodeNS"](newAttr);
                                              });
var h$webkit_dom_element_has_attribute;
h$webkit_dom_element_has_attribute = (function(self,
                                      self_2, name, name_2)
                                      {
                                        return self["hasAttribute"](h$decodeUtf8z(name,
                                        name_2));
                                      });
var h$webkit_dom_element_has_attribute_ns;
h$webkit_dom_element_has_attribute_ns = (function(self,
                                         self_2, namespaceURI,
                                         namespaceURI_2, localName,
                                         localName_2)
                                         {
                                           return self["hasAttributeNS"](h$decodeUtf8z(namespaceURI,
                                           namespaceURI_2),
                                           h$decodeUtf8z(localName,
                                           localName_2));
                                         });
var h$webkit_dom_element_focus;
h$webkit_dom_element_focus = (function(self,
                              self_2)
                              {
                                return self["focus"]();
                              });
var h$webkit_dom_element_blur;
h$webkit_dom_element_blur = (function(self,
                             self_2)
                             {
                               return self["blur"]();
                             });
var h$webkit_dom_element_scroll_into_view;
h$webkit_dom_element_scroll_into_view = (function(self,
                                         self_2, alignWithTop)
                                         {
                                           return self["scrollIntoView"](alignWithTop);
                                         });
var h$webkit_dom_element_scroll_into_view_if_needed;
h$webkit_dom_element_scroll_into_view_if_needed = (function(self,
                                                   self_2, centerIfNeeded)
                                                   {
                                                     return self["scrollIntoViewIfNeeded"](centerIfNeeded);
                                                   });
var h$webkit_dom_element_scroll_by_lines;
h$webkit_dom_element_scroll_by_lines = (function(self,
                                        self_2, lines)
                                        {
                                          return self["scrollByLines"](lines);
                                        });
var h$webkit_dom_element_scroll_by_pages;
h$webkit_dom_element_scroll_by_pages = (function(self,
                                        self_2, pages)
                                        {
                                          return self["scrollByPages"](pages);
                                        });
var h$webkit_dom_element_get_elements_by_class_name;
h$webkit_dom_element_get_elements_by_class_name = (function(self,
                                                   self_2, name, name_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["getElementsByClassName"](h$decodeUtf8z(name,
                                                     name_2));
                                                   });
var h$webkit_dom_element_query_selector;
h$webkit_dom_element_query_selector = (function(self,
                                       self_2, selectors, selectors_2)
                                       {
                                         h$ret1 = 0;
                                         return self["querySelector"](h$decodeUtf8z(selectors,
                                         selectors_2));
                                       });
var h$webkit_dom_element_query_selector_all;
h$webkit_dom_element_query_selector_all = (function(self,
                                           self_2, selectors, selectors_2)
                                           {
                                             h$ret1 = 0;
                                             return self["querySelectorAll"](h$decodeUtf8z(selectors,
                                             selectors_2));
                                           });
var h$webkit_dom_element_webkit_matches_selector;
h$webkit_dom_element_webkit_matches_selector = (function(self,
                                                self_2, selectors, selectors_2)
                                                {
                                                  return self["webkitMatchesSelector"](h$decodeUtf8z(selectors,
                                                  selectors_2));
                                                });
var h$webkit_dom_element_get_tag_name;
h$webkit_dom_element_get_tag_name = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return h$encodeUtf8(self["tagName"]);
                                     });
var h$webkit_dom_element_get_style;
h$webkit_dom_element_get_style = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return self["style"];
                                  });
var h$webkit_dom_element_get_offset_left;
h$webkit_dom_element_get_offset_left = (function(self,
                                        self_2)
                                        {
                                          return self["offsetLeft"];
                                        });
var h$webkit_dom_element_get_offset_top;
h$webkit_dom_element_get_offset_top = (function(self,
                                       self_2)
                                       {
                                         return self["offsetTop"];
                                       });
var h$webkit_dom_element_get_offset_width;
h$webkit_dom_element_get_offset_width = (function(self,
                                         self_2)
                                         {
                                           return self["offsetWidth"];
                                         });
var h$webkit_dom_element_get_offset_height;
h$webkit_dom_element_get_offset_height = (function(self,
                                          self_2)
                                          {
                                            return self["offsetHeight"];
                                          });
var h$webkit_dom_element_get_offset_parent;
h$webkit_dom_element_get_offset_parent = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["offsetParent"];
                                          });
var h$webkit_dom_element_get_client_left;
h$webkit_dom_element_get_client_left = (function(self,
                                        self_2)
                                        {
                                          return self["clientLeft"];
                                        });
var h$webkit_dom_element_get_client_top;
h$webkit_dom_element_get_client_top = (function(self,
                                       self_2)
                                       {
                                         return self["clientTop"];
                                       });
var h$webkit_dom_element_get_client_width;
h$webkit_dom_element_get_client_width = (function(self,
                                         self_2)
                                         {
                                           return self["clientWidth"];
                                         });
var h$webkit_dom_element_get_client_height;
h$webkit_dom_element_get_client_height = (function(self,
                                          self_2)
                                          {
                                            return self["clientHeight"];
                                          });
var h$webkit_dom_element_set_scroll_left;
h$webkit_dom_element_set_scroll_left = (function(self,
                                        self_2, val)
                                        {
                                          self["scrollLeft"] = val;
                                        });
var h$webkit_dom_element_get_scroll_left;
h$webkit_dom_element_get_scroll_left = (function(self,
                                        self_2)
                                        {
                                          return self["scrollLeft"];
                                        });
var h$webkit_dom_element_set_scroll_top;
h$webkit_dom_element_set_scroll_top = (function(self,
                                       self_2, val)
                                       {
                                         self["scrollTop"] = val;
                                       });
var h$webkit_dom_element_get_scroll_top;
h$webkit_dom_element_get_scroll_top = (function(self,
                                       self_2)
                                       {
                                         return self["scrollTop"];
                                       });
var h$webkit_dom_element_get_scroll_width;
h$webkit_dom_element_get_scroll_width = (function(self,
                                         self_2)
                                         {
                                           return self["scrollWidth"];
                                         });
var h$webkit_dom_element_get_scroll_height;
h$webkit_dom_element_get_scroll_height = (function(self,
                                          self_2)
                                          {
                                            return self["scrollHeight"];
                                          });
var h$webkit_dom_element_set_class_name;
h$webkit_dom_element_set_class_name = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["className"] = h$decodeUtf8z(val,
                                         val_2);
                                       });
var h$webkit_dom_element_get_class_name;
h$webkit_dom_element_get_class_name = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["className"]);
                                       });
var h$webkit_dom_element_get_class_list;
h$webkit_dom_element_get_class_list = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["classList"];
                                       });
var h$webkit_dom_element_get_first_element_child;
h$webkit_dom_element_get_first_element_child = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["firstElementChild"];
                                                });
var h$webkit_dom_element_get_last_element_child;
h$webkit_dom_element_get_last_element_child = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["lastElementChild"];
                                               });
var h$webkit_dom_element_get_previous_element_sibling;
h$webkit_dom_element_get_previous_element_sibling = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["previousElementSibling"];
                                                     });
var h$webkit_dom_element_get_next_element_sibling;
h$webkit_dom_element_get_next_element_sibling = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["nextElementSibling"];
                                                 });
var h$webkit_dom_element_get_child_element_count;
h$webkit_dom_element_get_child_element_count = (function(self,
                                                self_2)
                                                {
                                                  return self["childElementCount"];
                                                });
var h$webkit_dom_element_get_webkit_region_overset;
h$webkit_dom_element_get_webkit_region_overset = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["webkitRegionOverset"]);
                                                  });
var h$webkit_dom_element_set_onabort;
h$webkit_dom_element_set_onabort = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onabort"] = val;
                                    });
var h$webkit_dom_element_get_onabort;
h$webkit_dom_element_get_onabort = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onabort"];
                                    });
var h$webkit_dom_element_set_onblur;
h$webkit_dom_element_set_onblur = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["onblur"] = val;
                                   });
var h$webkit_dom_element_get_onblur;
h$webkit_dom_element_get_onblur = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["onblur"];
                                   });
var h$webkit_dom_element_set_onchange;
h$webkit_dom_element_set_onchange = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onchange"] = val;
                                     });
var h$webkit_dom_element_get_onchange;
h$webkit_dom_element_get_onchange = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onchange"];
                                     });
var h$webkit_dom_element_set_onclick;
h$webkit_dom_element_set_onclick = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onclick"] = val;
                                    });
var h$webkit_dom_element_get_onclick;
h$webkit_dom_element_get_onclick = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onclick"];
                                    });
var h$webkit_dom_element_set_oncontextmenu;
h$webkit_dom_element_set_oncontextmenu = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["oncontextmenu"] = val;
                                          });
var h$webkit_dom_element_get_oncontextmenu;
h$webkit_dom_element_get_oncontextmenu = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["oncontextmenu"];
                                          });
var h$webkit_dom_element_set_ondblclick;
h$webkit_dom_element_set_ondblclick = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["ondblclick"] = val;
                                       });
var h$webkit_dom_element_get_ondblclick;
h$webkit_dom_element_get_ondblclick = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["ondblclick"];
                                       });
var h$webkit_dom_element_set_ondrag;
h$webkit_dom_element_set_ondrag = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["ondrag"] = val;
                                   });
var h$webkit_dom_element_get_ondrag;
h$webkit_dom_element_get_ondrag = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["ondrag"];
                                   });
var h$webkit_dom_element_set_ondragend;
h$webkit_dom_element_set_ondragend = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["ondragend"] = val;
                                      });
var h$webkit_dom_element_get_ondragend;
h$webkit_dom_element_get_ondragend = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["ondragend"];
                                      });
var h$webkit_dom_element_set_ondragenter;
h$webkit_dom_element_set_ondragenter = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ondragenter"] = val;
                                        });
var h$webkit_dom_element_get_ondragenter;
h$webkit_dom_element_get_ondragenter = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ondragenter"];
                                        });
var h$webkit_dom_element_set_ondragleave;
h$webkit_dom_element_set_ondragleave = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ondragleave"] = val;
                                        });
var h$webkit_dom_element_get_ondragleave;
h$webkit_dom_element_get_ondragleave = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ondragleave"];
                                        });
var h$webkit_dom_element_set_ondragover;
h$webkit_dom_element_set_ondragover = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["ondragover"] = val;
                                       });
var h$webkit_dom_element_get_ondragover;
h$webkit_dom_element_get_ondragover = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["ondragover"];
                                       });
var h$webkit_dom_element_set_ondragstart;
h$webkit_dom_element_set_ondragstart = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ondragstart"] = val;
                                        });
var h$webkit_dom_element_get_ondragstart;
h$webkit_dom_element_get_ondragstart = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ondragstart"];
                                        });
var h$webkit_dom_element_set_ondrop;
h$webkit_dom_element_set_ondrop = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["ondrop"] = val;
                                   });
var h$webkit_dom_element_get_ondrop;
h$webkit_dom_element_get_ondrop = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["ondrop"];
                                   });
var h$webkit_dom_element_set_onerror;
h$webkit_dom_element_set_onerror = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onerror"] = val;
                                    });
var h$webkit_dom_element_get_onerror;
h$webkit_dom_element_get_onerror = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onerror"];
                                    });
var h$webkit_dom_element_set_onfocus;
h$webkit_dom_element_set_onfocus = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onfocus"] = val;
                                    });
var h$webkit_dom_element_get_onfocus;
h$webkit_dom_element_get_onfocus = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onfocus"];
                                    });
var h$webkit_dom_element_set_oninput;
h$webkit_dom_element_set_oninput = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["oninput"] = val;
                                    });
var h$webkit_dom_element_get_oninput;
h$webkit_dom_element_get_oninput = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["oninput"];
                                    });
var h$webkit_dom_element_set_oninvalid;
h$webkit_dom_element_set_oninvalid = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["oninvalid"] = val;
                                      });
var h$webkit_dom_element_get_oninvalid;
h$webkit_dom_element_get_oninvalid = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["oninvalid"];
                                      });
var h$webkit_dom_element_set_onkeydown;
h$webkit_dom_element_set_onkeydown = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onkeydown"] = val;
                                      });
var h$webkit_dom_element_get_onkeydown;
h$webkit_dom_element_get_onkeydown = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onkeydown"];
                                      });
var h$webkit_dom_element_set_onkeypress;
h$webkit_dom_element_set_onkeypress = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onkeypress"] = val;
                                       });
var h$webkit_dom_element_get_onkeypress;
h$webkit_dom_element_get_onkeypress = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onkeypress"];
                                       });
var h$webkit_dom_element_set_onkeyup;
h$webkit_dom_element_set_onkeyup = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onkeyup"] = val;
                                    });
var h$webkit_dom_element_get_onkeyup;
h$webkit_dom_element_get_onkeyup = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onkeyup"];
                                    });
var h$webkit_dom_element_set_onload;
h$webkit_dom_element_set_onload = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["onload"] = val;
                                   });
var h$webkit_dom_element_get_onload;
h$webkit_dom_element_get_onload = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["onload"];
                                   });
var h$webkit_dom_element_set_onmousedown;
h$webkit_dom_element_set_onmousedown = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onmousedown"] = val;
                                        });
var h$webkit_dom_element_get_onmousedown;
h$webkit_dom_element_get_onmousedown = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onmousedown"];
                                        });
var h$webkit_dom_element_set_onmousemove;
h$webkit_dom_element_set_onmousemove = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onmousemove"] = val;
                                        });
var h$webkit_dom_element_get_onmousemove;
h$webkit_dom_element_get_onmousemove = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onmousemove"];
                                        });
var h$webkit_dom_element_set_onmouseout;
h$webkit_dom_element_set_onmouseout = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onmouseout"] = val;
                                       });
var h$webkit_dom_element_get_onmouseout;
h$webkit_dom_element_get_onmouseout = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onmouseout"];
                                       });
var h$webkit_dom_element_set_onmouseover;
h$webkit_dom_element_set_onmouseover = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onmouseover"] = val;
                                        });
var h$webkit_dom_element_get_onmouseover;
h$webkit_dom_element_get_onmouseover = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onmouseover"];
                                        });
var h$webkit_dom_element_set_onmouseup;
h$webkit_dom_element_set_onmouseup = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onmouseup"] = val;
                                      });
var h$webkit_dom_element_get_onmouseup;
h$webkit_dom_element_get_onmouseup = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onmouseup"];
                                      });
var h$webkit_dom_element_set_onmousewheel;
h$webkit_dom_element_set_onmousewheel = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onmousewheel"] = val;
                                         });
var h$webkit_dom_element_get_onmousewheel;
h$webkit_dom_element_get_onmousewheel = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onmousewheel"];
                                         });
var h$webkit_dom_element_set_onscroll;
h$webkit_dom_element_set_onscroll = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onscroll"] = val;
                                     });
var h$webkit_dom_element_get_onscroll;
h$webkit_dom_element_get_onscroll = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onscroll"];
                                     });
var h$webkit_dom_element_set_onselect;
h$webkit_dom_element_set_onselect = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onselect"] = val;
                                     });
var h$webkit_dom_element_get_onselect;
h$webkit_dom_element_get_onselect = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onselect"];
                                     });
var h$webkit_dom_element_set_onsubmit;
h$webkit_dom_element_set_onsubmit = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onsubmit"] = val;
                                     });
var h$webkit_dom_element_get_onsubmit;
h$webkit_dom_element_get_onsubmit = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onsubmit"];
                                     });
var h$webkit_dom_element_set_onbeforecut;
h$webkit_dom_element_set_onbeforecut = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onbeforecut"] = val;
                                        });
var h$webkit_dom_element_get_onbeforecut;
h$webkit_dom_element_get_onbeforecut = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onbeforecut"];
                                        });
var h$webkit_dom_element_set_oncut;
h$webkit_dom_element_set_oncut = (function(self,
                                  self_2, val, val_2)
                                  {
                                    self["oncut"] = val;
                                  });
var h$webkit_dom_element_get_oncut;
h$webkit_dom_element_get_oncut = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return self["oncut"];
                                  });
var h$webkit_dom_element_set_onbeforecopy;
h$webkit_dom_element_set_onbeforecopy = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onbeforecopy"] = val;
                                         });
var h$webkit_dom_element_get_onbeforecopy;
h$webkit_dom_element_get_onbeforecopy = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onbeforecopy"];
                                         });
var h$webkit_dom_element_set_oncopy;
h$webkit_dom_element_set_oncopy = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["oncopy"] = val;
                                   });
var h$webkit_dom_element_get_oncopy;
h$webkit_dom_element_get_oncopy = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["oncopy"];
                                   });
var h$webkit_dom_element_set_onbeforepaste;
h$webkit_dom_element_set_onbeforepaste = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onbeforepaste"] = val;
                                          });
var h$webkit_dom_element_get_onbeforepaste;
h$webkit_dom_element_get_onbeforepaste = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onbeforepaste"];
                                          });
var h$webkit_dom_element_set_onpaste;
h$webkit_dom_element_set_onpaste = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onpaste"] = val;
                                    });
var h$webkit_dom_element_get_onpaste;
h$webkit_dom_element_get_onpaste = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onpaste"];
                                    });
var h$webkit_dom_element_set_onreset;
h$webkit_dom_element_set_onreset = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onreset"] = val;
                                    });
var h$webkit_dom_element_get_onreset;
h$webkit_dom_element_get_onreset = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onreset"];
                                    });
var h$webkit_dom_element_set_onsearch;
h$webkit_dom_element_set_onsearch = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onsearch"] = val;
                                     });
var h$webkit_dom_element_get_onsearch;
h$webkit_dom_element_get_onsearch = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onsearch"];
                                     });
var h$webkit_dom_element_set_onselectstart;
h$webkit_dom_element_set_onselectstart = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onselectstart"] = val;
                                          });
var h$webkit_dom_element_get_onselectstart;
h$webkit_dom_element_get_onselectstart = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onselectstart"];
                                          });
var h$webkit_dom_element_set_ontouchstart;
h$webkit_dom_element_set_ontouchstart = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["ontouchstart"] = val;
                                         });
var h$webkit_dom_element_get_ontouchstart;
h$webkit_dom_element_get_ontouchstart = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["ontouchstart"];
                                         });
var h$webkit_dom_element_set_ontouchmove;
h$webkit_dom_element_set_ontouchmove = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ontouchmove"] = val;
                                        });
var h$webkit_dom_element_get_ontouchmove;
h$webkit_dom_element_get_ontouchmove = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ontouchmove"];
                                        });
var h$webkit_dom_element_set_ontouchend;
h$webkit_dom_element_set_ontouchend = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["ontouchend"] = val;
                                       });
var h$webkit_dom_element_get_ontouchend;
h$webkit_dom_element_get_ontouchend = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["ontouchend"];
                                       });
var h$webkit_dom_element_set_ontouchcancel;
h$webkit_dom_element_set_ontouchcancel = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["ontouchcancel"] = val;
                                          });
var h$webkit_dom_element_get_ontouchcancel;
h$webkit_dom_element_get_ontouchcancel = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["ontouchcancel"];
                                          });
var h$webkit_dom_element_set_onwebkitfullscreenchange;
h$webkit_dom_element_set_onwebkitfullscreenchange = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onwebkitfullscreenchange"] = val;
                                                     });
var h$webkit_dom_element_get_onwebkitfullscreenchange;
h$webkit_dom_element_get_onwebkitfullscreenchange = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onwebkitfullscreenchange"];
                                                     });
var h$webkit_dom_element_set_onwebkitfullscreenerror;
h$webkit_dom_element_set_onwebkitfullscreenerror = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["onwebkitfullscreenerror"] = val;
                                                    });
var h$webkit_dom_element_get_onwebkitfullscreenerror;
h$webkit_dom_element_get_onwebkitfullscreenerror = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["onwebkitfullscreenerror"];
                                                    });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_dom_window_get_type = (function()
                                    {
                                      return h$g_get_type(Window);
                                    });
var h$webkit_dom_dom_window_get_selection;
h$webkit_dom_dom_window_get_selection = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["getSelection"]();
                                         });
var h$webkit_dom_dom_window_blur;
h$webkit_dom_dom_window_blur = (function(self,
                                self_2)
                                {
                                  return self["blur"]();
                                });
var h$webkit_dom_dom_window_print;
h$webkit_dom_dom_window_print = (function(self,
                                 self_2)
                                 {
                                   return self["print"]();
                                 });
var h$webkit_dom_dom_window_stop;
h$webkit_dom_dom_window_stop = (function(self,
                                self_2)
                                {
                                  return self["stop"]();
                                });
var h$webkit_dom_dom_window_alert;
h$webkit_dom_dom_window_alert = (function(self,
                                 self_2, message, message_2)
                                 {
                                   return self["alert"](h$decodeUtf8z(message,
                                   message_2));
                                 });
var h$webkit_dom_dom_window_confirm;
h$webkit_dom_dom_window_confirm = (function(self,
                                   self_2, message, message_2)
                                   {
                                     return self["confirm"](h$decodeUtf8z(message,
                                     message_2));
                                   });
var h$webkit_dom_dom_window_prompt;
h$webkit_dom_dom_window_prompt = (function(self,
                                  self_2, message, message_2,
                                  defaultValue, defaultValue_2)
                                  {
                                    h$ret1 = 0;
                                    return h$encodeUtf8(self["prompt"](h$decodeUtf8z(message,
                                    message_2),
                                    h$decodeUtf8z(defaultValue,
                                    defaultValue_2)));
                                  });
var h$webkit_dom_dom_window_find;
h$webkit_dom_dom_window_find = (function(self,
                                self_2, string, string_2,
                                caseSensitive, backwards, wrap,
                                wholeWord, searchInFrames,
                                showDialog)
                                {
                                  return self["find"](h$decodeUtf8z(string,
                                  string_2), caseSensitive,
                                  backwards, wrap, wholeWord,
                                  searchInFrames, showDialog);
                                });
var h$webkit_dom_dom_window_scroll_by;
h$webkit_dom_dom_window_scroll_by = (function(self,
                                     self_2, x, y)
                                     {
                                       return self["scrollBy"](x, y);
                                     });
var h$webkit_dom_dom_window_scroll_to;
h$webkit_dom_dom_window_scroll_to = (function(self,
                                     self_2, x, y)
                                     {
                                       return self["scrollTo"](x, y);
                                     });
var h$webkit_dom_dom_window_scroll;
h$webkit_dom_dom_window_scroll = (function(self,
                                  self_2, x, y)
                                  {
                                    return self["scroll"](x, y);
                                  });
var h$webkit_dom_dom_window_move_by;
h$webkit_dom_dom_window_move_by = (function(self,
                                   self_2, x, y)
                                   {
                                     return self["moveBy"](x, y);
                                   });
var h$webkit_dom_dom_window_move_to;
h$webkit_dom_dom_window_move_to = (function(self,
                                   self_2, x, y)
                                   {
                                     return self["moveTo"](x, y);
                                   });
var h$webkit_dom_dom_window_resize_by;
h$webkit_dom_dom_window_resize_by = (function(self,
                                     self_2, x, y)
                                     {
                                       return self["resizeBy"](x, y);
                                     });
var h$webkit_dom_dom_window_resize_to;
h$webkit_dom_dom_window_resize_to = (function(self,
                                     self_2, width, height)
                                     {
                                       return self["resizeTo"](width,
                                       height);
                                     });
var h$webkit_dom_dom_window_match_media;
h$webkit_dom_dom_window_match_media = (function(self,
                                       self_2, query, query_2)
                                       {
                                         h$ret1 = 0;
                                         return self["matchMedia"](h$decodeUtf8z(query,
                                         query_2));
                                       });
var h$webkit_dom_dom_window_get_computed_style;
h$webkit_dom_dom_window_get_computed_style = (function(self,
                                              self_2, element, element_2,
                                              pseudoElement, pseudoElement_2)
                                              {
                                                h$ret1 = 0;
                                                return self["getComputedStyle"](element,
                                                h$decodeUtf8z(pseudoElement,
                                                pseudoElement_2));
                                              });
var h$webkit_dom_dom_window_webkit_convert_point_from_page_to_node;
h$webkit_dom_dom_window_webkit_convert_point_from_page_to_node = (function(self,
                                                                  self_2, node,
                                                                  node_2, p,
                                                                  p_2)
                                                                  {
                                                                    h$ret1 = 0;
                                                                    return self["webkitConvertPointFromPageToNode"](node,
                                                                    p);
                                                                  });
var h$webkit_dom_dom_window_webkit_convert_point_from_node_to_page;
h$webkit_dom_dom_window_webkit_convert_point_from_node_to_page = (function(self,
                                                                  self_2, node,
                                                                  node_2, p,
                                                                  p_2)
                                                                  {
                                                                    h$ret1 = 0;
                                                                    return self["webkitConvertPointFromNodeToPage"](node,
                                                                    p);
                                                                  });
var h$webkit_dom_dom_window_clear_timeout;
h$webkit_dom_dom_window_clear_timeout = (function(self,
                                         self_2, handle)
                                         {
                                           return self["clearTimeout"](handle);
                                         });
var h$webkit_dom_dom_window_clear_interval;
h$webkit_dom_dom_window_clear_interval = (function(self,
                                          self_2, handle)
                                          {
                                            return self["clearInterval"](handle);
                                          });
var h$webkit_dom_dom_window_atob;
h$webkit_dom_dom_window_atob = (function(self,
                                self_2, string, string_2)
                                {
                                  h$ret1 = 0;
                                  return h$encodeUtf8(self["atob"](h$decodeUtf8z(string,
                                  string_2)));
                                });
var h$webkit_dom_dom_window_btoa;
h$webkit_dom_dom_window_btoa = (function(self,
                                self_2, string, string_2)
                                {
                                  h$ret1 = 0;
                                  return h$encodeUtf8(self["btoa"](h$decodeUtf8z(string,
                                  string_2)));
                                });
var h$webkit_dom_dom_window_dispatch_event;
h$webkit_dom_dom_window_dispatch_event = (function(self,
                                          self_2, evt, evt_2)
                                          {
                                            return self["dispatchEvent"](evt);
                                          });
var h$webkit_dom_dom_window_capture_events;
h$webkit_dom_dom_window_capture_events = (function(self,
                                          self_2)
                                          {
                                            return self["captureEvents"]();
                                          });
var h$webkit_dom_dom_window_release_events;
h$webkit_dom_dom_window_release_events = (function(self,
                                          self_2)
                                          {
                                            return self["releaseEvents"]();
                                          });
var h$webkit_dom_dom_window_get_screen;
h$webkit_dom_dom_window_get_screen = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["screen"];
                                      });
var h$webkit_dom_dom_window_get_history;
h$webkit_dom_dom_window_get_history = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["history"];
                                       });
var h$webkit_dom_dom_window_get_locationbar;
h$webkit_dom_dom_window_get_locationbar = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["locationbar"];
                                           });
var h$webkit_dom_dom_window_get_menubar;
h$webkit_dom_dom_window_get_menubar = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["menubar"];
                                       });
var h$webkit_dom_dom_window_get_personalbar;
h$webkit_dom_dom_window_get_personalbar = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["personalbar"];
                                           });
var h$webkit_dom_dom_window_get_scrollbars;
h$webkit_dom_dom_window_get_scrollbars = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["scrollbars"];
                                          });
var h$webkit_dom_dom_window_get_statusbar;
h$webkit_dom_dom_window_get_statusbar = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["statusbar"];
                                         });
var h$webkit_dom_dom_window_get_toolbar;
h$webkit_dom_dom_window_get_toolbar = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["toolbar"];
                                       });
var h$webkit_dom_dom_window_get_navigator;
h$webkit_dom_dom_window_get_navigator = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["navigator"];
                                         });
var h$webkit_dom_dom_window_get_client_information;
h$webkit_dom_dom_window_get_client_information = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["clientInformation"];
                                                  });
var h$webkit_dom_dom_window_get_frame_element;
h$webkit_dom_dom_window_get_frame_element = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["frameElement"];
                                             });
var h$webkit_dom_dom_window_get_offscreen_buffering;
h$webkit_dom_dom_window_get_offscreen_buffering = (function(self,
                                                   self_2)
                                                   {
                                                     return self["offscreenBuffering"];
                                                   });
var h$webkit_dom_dom_window_get_outer_height;
h$webkit_dom_dom_window_get_outer_height = (function(self,
                                            self_2)
                                            {
                                              return self["outerHeight"];
                                            });
var h$webkit_dom_dom_window_get_outer_width;
h$webkit_dom_dom_window_get_outer_width = (function(self,
                                           self_2)
                                           {
                                             return self["outerWidth"];
                                           });
var h$webkit_dom_dom_window_get_inner_height;
h$webkit_dom_dom_window_get_inner_height = (function(self,
                                            self_2)
                                            {
                                              return self["innerHeight"];
                                            });
var h$webkit_dom_dom_window_get_inner_width;
h$webkit_dom_dom_window_get_inner_width = (function(self,
                                           self_2)
                                           {
                                             return self["innerWidth"];
                                           });
var h$webkit_dom_dom_window_get_screen_x;
h$webkit_dom_dom_window_get_screen_x = (function(self,
                                        self_2)
                                        {
                                          return self["screenX"];
                                        });
var h$webkit_dom_dom_window_get_screen_y;
h$webkit_dom_dom_window_get_screen_y = (function(self,
                                        self_2)
                                        {
                                          return self["screenY"];
                                        });
var h$webkit_dom_dom_window_get_screen_left;
h$webkit_dom_dom_window_get_screen_left = (function(self,
                                           self_2)
                                           {
                                             return self["screenLeft"];
                                           });
var h$webkit_dom_dom_window_get_screen_top;
h$webkit_dom_dom_window_get_screen_top = (function(self,
                                          self_2)
                                          {
                                            return self["screenTop"];
                                          });
var h$webkit_dom_dom_window_get_scroll_x;
h$webkit_dom_dom_window_get_scroll_x = (function(self,
                                        self_2)
                                        {
                                          return self["scrollX"];
                                        });
var h$webkit_dom_dom_window_get_scroll_y;
h$webkit_dom_dom_window_get_scroll_y = (function(self,
                                        self_2)
                                        {
                                          return self["scrollY"];
                                        });
var h$webkit_dom_dom_window_get_page_x_offset;
h$webkit_dom_dom_window_get_page_x_offset = (function(self,
                                             self_2)
                                             {
                                               return self["pageXOffset"];
                                             });
var h$webkit_dom_dom_window_get_page_y_offset;
h$webkit_dom_dom_window_get_page_y_offset = (function(self,
                                             self_2)
                                             {
                                               return self["pageYOffset"];
                                             });
var h$webkit_dom_dom_window_get_closed;
h$webkit_dom_dom_window_get_closed = (function(self,
                                      self_2)
                                      {
                                        return self["closed"];
                                      });
var h$webkit_dom_dom_window_get_length;
h$webkit_dom_dom_window_get_length = (function(self,
                                      self_2)
                                      {
                                        return self["length"];
                                      });
var h$webkit_dom_dom_window_set_name;
h$webkit_dom_dom_window_set_name = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["name"] = h$decodeUtf8z(val,
                                      val_2);
                                    });
var h$webkit_dom_dom_window_get_name;
h$webkit_dom_dom_window_get_name = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["name"]);
                                    });
var h$webkit_dom_dom_window_set_status;
h$webkit_dom_dom_window_set_status = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["status"] = h$decodeUtf8z(val,
                                        val_2);
                                      });
var h$webkit_dom_dom_window_get_status;
h$webkit_dom_dom_window_get_status = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["status"]);
                                      });
var h$webkit_dom_dom_window_set_default_status;
h$webkit_dom_dom_window_set_default_status = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["defaultStatus"] = h$decodeUtf8z(val,
                                                val_2);
                                              });
var h$webkit_dom_dom_window_get_default_status;
h$webkit_dom_dom_window_get_default_status = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["defaultStatus"]);
                                              });
var h$webkit_dom_dom_window_get_self;
h$webkit_dom_dom_window_get_self = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["self"];
                                    });
var h$webkit_dom_dom_window_get_window;
h$webkit_dom_dom_window_get_window = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["window"];
                                      });
var h$webkit_dom_dom_window_get_frames;
h$webkit_dom_dom_window_get_frames = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["frames"];
                                      });
var h$webkit_dom_dom_window_get_opener;
h$webkit_dom_dom_window_get_opener = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["opener"];
                                      });
var h$webkit_dom_dom_window_get_parent;
h$webkit_dom_dom_window_get_parent = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["parent"];
                                      });
var h$webkit_dom_dom_window_get_top;
h$webkit_dom_dom_window_get_top = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["top"];
                                   });
var h$webkit_dom_dom_window_get_document;
h$webkit_dom_dom_window_get_document = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["document"];
                                        });
var h$webkit_dom_dom_window_get_style_media;
h$webkit_dom_dom_window_get_style_media = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["styleMedia"];
                                           });
var h$webkit_dom_dom_window_get_device_pixel_ratio;
h$webkit_dom_dom_window_get_device_pixel_ratio = (function(self,
                                                  self_2)
                                                  {
                                                    return self["devicePixelRatio"];
                                                  });
var h$webkit_dom_dom_window_get_application_cache;
h$webkit_dom_dom_window_get_application_cache = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["applicationCache"];
                                                 });
var h$webkit_dom_dom_window_get_session_storage;
h$webkit_dom_dom_window_get_session_storage = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["sessionStorage"];
                                               });
var h$webkit_dom_dom_window_get_local_storage;
h$webkit_dom_dom_window_get_local_storage = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["localStorage"];
                                             });
var h$webkit_dom_dom_window_get_console;
h$webkit_dom_dom_window_get_console = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["console"];
                                       });
var h$webkit_dom_dom_window_set_onabort;
h$webkit_dom_dom_window_set_onabort = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onabort"] = val;
                                       });
var h$webkit_dom_dom_window_get_onabort;
h$webkit_dom_dom_window_get_onabort = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onabort"];
                                       });
var h$webkit_dom_dom_window_set_onbeforeunload;
h$webkit_dom_dom_window_set_onbeforeunload = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["onbeforeunload"] = val;
                                              });
var h$webkit_dom_dom_window_get_onbeforeunload;
h$webkit_dom_dom_window_get_onbeforeunload = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["onbeforeunload"];
                                              });
var h$webkit_dom_dom_window_set_onblur;
h$webkit_dom_dom_window_set_onblur = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onblur"] = val;
                                      });
var h$webkit_dom_dom_window_get_onblur;
h$webkit_dom_dom_window_get_onblur = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onblur"];
                                      });
var h$webkit_dom_dom_window_set_oncanplay;
h$webkit_dom_dom_window_set_oncanplay = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["oncanplay"] = val;
                                         });
var h$webkit_dom_dom_window_get_oncanplay;
h$webkit_dom_dom_window_get_oncanplay = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["oncanplay"];
                                         });
var h$webkit_dom_dom_window_set_oncanplaythrough;
h$webkit_dom_dom_window_set_oncanplaythrough = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["oncanplaythrough"] = val;
                                                });
var h$webkit_dom_dom_window_get_oncanplaythrough;
h$webkit_dom_dom_window_get_oncanplaythrough = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["oncanplaythrough"];
                                                });
var h$webkit_dom_dom_window_set_onchange;
h$webkit_dom_dom_window_set_onchange = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onchange"] = val;
                                        });
var h$webkit_dom_dom_window_get_onchange;
h$webkit_dom_dom_window_get_onchange = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onchange"];
                                        });
var h$webkit_dom_dom_window_set_onclick;
h$webkit_dom_dom_window_set_onclick = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onclick"] = val;
                                       });
var h$webkit_dom_dom_window_get_onclick;
h$webkit_dom_dom_window_get_onclick = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onclick"];
                                       });
var h$webkit_dom_dom_window_set_oncontextmenu;
h$webkit_dom_dom_window_set_oncontextmenu = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["oncontextmenu"] = val;
                                             });
var h$webkit_dom_dom_window_get_oncontextmenu;
h$webkit_dom_dom_window_get_oncontextmenu = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["oncontextmenu"];
                                             });
var h$webkit_dom_dom_window_set_ondblclick;
h$webkit_dom_dom_window_set_ondblclick = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["ondblclick"] = val;
                                          });
var h$webkit_dom_dom_window_get_ondblclick;
h$webkit_dom_dom_window_get_ondblclick = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["ondblclick"];
                                          });
var h$webkit_dom_dom_window_set_ondrag;
h$webkit_dom_dom_window_set_ondrag = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["ondrag"] = val;
                                      });
var h$webkit_dom_dom_window_get_ondrag;
h$webkit_dom_dom_window_get_ondrag = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["ondrag"];
                                      });
var h$webkit_dom_dom_window_set_ondragend;
h$webkit_dom_dom_window_set_ondragend = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["ondragend"] = val;
                                         });
var h$webkit_dom_dom_window_get_ondragend;
h$webkit_dom_dom_window_get_ondragend = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["ondragend"];
                                         });
var h$webkit_dom_dom_window_set_ondragenter;
h$webkit_dom_dom_window_set_ondragenter = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["ondragenter"] = val;
                                           });
var h$webkit_dom_dom_window_get_ondragenter;
h$webkit_dom_dom_window_get_ondragenter = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["ondragenter"];
                                           });
var h$webkit_dom_dom_window_set_ondragleave;
h$webkit_dom_dom_window_set_ondragleave = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["ondragleave"] = val;
                                           });
var h$webkit_dom_dom_window_get_ondragleave;
h$webkit_dom_dom_window_get_ondragleave = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["ondragleave"];
                                           });
var h$webkit_dom_dom_window_set_ondragover;
h$webkit_dom_dom_window_set_ondragover = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["ondragover"] = val;
                                          });
var h$webkit_dom_dom_window_get_ondragover;
h$webkit_dom_dom_window_get_ondragover = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["ondragover"];
                                          });
var h$webkit_dom_dom_window_set_ondragstart;
h$webkit_dom_dom_window_set_ondragstart = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["ondragstart"] = val;
                                           });
var h$webkit_dom_dom_window_get_ondragstart;
h$webkit_dom_dom_window_get_ondragstart = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["ondragstart"];
                                           });
var h$webkit_dom_dom_window_set_ondrop;
h$webkit_dom_dom_window_set_ondrop = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["ondrop"] = val;
                                      });
var h$webkit_dom_dom_window_get_ondrop;
h$webkit_dom_dom_window_get_ondrop = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["ondrop"];
                                      });
var h$webkit_dom_dom_window_set_ondurationchange;
h$webkit_dom_dom_window_set_ondurationchange = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["ondurationchange"] = val;
                                                });
var h$webkit_dom_dom_window_get_ondurationchange;
h$webkit_dom_dom_window_get_ondurationchange = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["ondurationchange"];
                                                });
var h$webkit_dom_dom_window_set_onemptied;
h$webkit_dom_dom_window_set_onemptied = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onemptied"] = val;
                                         });
var h$webkit_dom_dom_window_get_onemptied;
h$webkit_dom_dom_window_get_onemptied = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onemptied"];
                                         });
var h$webkit_dom_dom_window_set_onended;
h$webkit_dom_dom_window_set_onended = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onended"] = val;
                                       });
var h$webkit_dom_dom_window_get_onended;
h$webkit_dom_dom_window_get_onended = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onended"];
                                       });
var h$webkit_dom_dom_window_set_onerror;
h$webkit_dom_dom_window_set_onerror = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onerror"] = val;
                                       });
var h$webkit_dom_dom_window_get_onerror;
h$webkit_dom_dom_window_get_onerror = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onerror"];
                                       });
var h$webkit_dom_dom_window_set_onfocus;
h$webkit_dom_dom_window_set_onfocus = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onfocus"] = val;
                                       });
var h$webkit_dom_dom_window_get_onfocus;
h$webkit_dom_dom_window_get_onfocus = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onfocus"];
                                       });
var h$webkit_dom_dom_window_set_onhashchange;
h$webkit_dom_dom_window_set_onhashchange = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["onhashchange"] = val;
                                            });
var h$webkit_dom_dom_window_get_onhashchange;
h$webkit_dom_dom_window_get_onhashchange = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["onhashchange"];
                                            });
var h$webkit_dom_dom_window_set_oninput;
h$webkit_dom_dom_window_set_oninput = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["oninput"] = val;
                                       });
var h$webkit_dom_dom_window_get_oninput;
h$webkit_dom_dom_window_get_oninput = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["oninput"];
                                       });
var h$webkit_dom_dom_window_set_oninvalid;
h$webkit_dom_dom_window_set_oninvalid = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["oninvalid"] = val;
                                         });
var h$webkit_dom_dom_window_get_oninvalid;
h$webkit_dom_dom_window_get_oninvalid = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["oninvalid"];
                                         });
var h$webkit_dom_dom_window_set_onkeydown;
h$webkit_dom_dom_window_set_onkeydown = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onkeydown"] = val;
                                         });
var h$webkit_dom_dom_window_get_onkeydown;
h$webkit_dom_dom_window_get_onkeydown = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onkeydown"];
                                         });
var h$webkit_dom_dom_window_set_onkeypress;
h$webkit_dom_dom_window_set_onkeypress = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onkeypress"] = val;
                                          });
var h$webkit_dom_dom_window_get_onkeypress;
h$webkit_dom_dom_window_get_onkeypress = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onkeypress"];
                                          });
var h$webkit_dom_dom_window_set_onkeyup;
h$webkit_dom_dom_window_set_onkeyup = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onkeyup"] = val;
                                       });
var h$webkit_dom_dom_window_get_onkeyup;
h$webkit_dom_dom_window_get_onkeyup = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onkeyup"];
                                       });
var h$webkit_dom_dom_window_set_onload;
h$webkit_dom_dom_window_set_onload = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onload"] = val;
                                      });
var h$webkit_dom_dom_window_get_onload;
h$webkit_dom_dom_window_get_onload = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onload"];
                                      });
var h$webkit_dom_dom_window_set_onloadeddata;
h$webkit_dom_dom_window_set_onloadeddata = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["onloadeddata"] = val;
                                            });
var h$webkit_dom_dom_window_get_onloadeddata;
h$webkit_dom_dom_window_get_onloadeddata = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["onloadeddata"];
                                            });
var h$webkit_dom_dom_window_set_onloadedmetadata;
h$webkit_dom_dom_window_set_onloadedmetadata = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["onloadedmetadata"] = val;
                                                });
var h$webkit_dom_dom_window_get_onloadedmetadata;
h$webkit_dom_dom_window_get_onloadedmetadata = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["onloadedmetadata"];
                                                });
var h$webkit_dom_dom_window_set_onloadstart;
h$webkit_dom_dom_window_set_onloadstart = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["onloadstart"] = val;
                                           });
var h$webkit_dom_dom_window_get_onloadstart;
h$webkit_dom_dom_window_get_onloadstart = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["onloadstart"];
                                           });
var h$webkit_dom_dom_window_set_onmessage;
h$webkit_dom_dom_window_set_onmessage = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onmessage"] = val;
                                         });
var h$webkit_dom_dom_window_get_onmessage;
h$webkit_dom_dom_window_get_onmessage = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onmessage"];
                                         });
var h$webkit_dom_dom_window_set_onmousedown;
h$webkit_dom_dom_window_set_onmousedown = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["onmousedown"] = val;
                                           });
var h$webkit_dom_dom_window_get_onmousedown;
h$webkit_dom_dom_window_get_onmousedown = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["onmousedown"];
                                           });
var h$webkit_dom_dom_window_set_onmousemove;
h$webkit_dom_dom_window_set_onmousemove = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["onmousemove"] = val;
                                           });
var h$webkit_dom_dom_window_get_onmousemove;
h$webkit_dom_dom_window_get_onmousemove = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["onmousemove"];
                                           });
var h$webkit_dom_dom_window_set_onmouseout;
h$webkit_dom_dom_window_set_onmouseout = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onmouseout"] = val;
                                          });
var h$webkit_dom_dom_window_get_onmouseout;
h$webkit_dom_dom_window_get_onmouseout = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onmouseout"];
                                          });
var h$webkit_dom_dom_window_set_onmouseover;
h$webkit_dom_dom_window_set_onmouseover = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["onmouseover"] = val;
                                           });
var h$webkit_dom_dom_window_get_onmouseover;
h$webkit_dom_dom_window_get_onmouseover = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["onmouseover"];
                                           });
var h$webkit_dom_dom_window_set_onmouseup;
h$webkit_dom_dom_window_set_onmouseup = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onmouseup"] = val;
                                         });
var h$webkit_dom_dom_window_get_onmouseup;
h$webkit_dom_dom_window_get_onmouseup = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onmouseup"];
                                         });
var h$webkit_dom_dom_window_set_onmousewheel;
h$webkit_dom_dom_window_set_onmousewheel = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["onmousewheel"] = val;
                                            });
var h$webkit_dom_dom_window_get_onmousewheel;
h$webkit_dom_dom_window_get_onmousewheel = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["onmousewheel"];
                                            });
var h$webkit_dom_dom_window_set_onoffline;
h$webkit_dom_dom_window_set_onoffline = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onoffline"] = val;
                                         });
var h$webkit_dom_dom_window_get_onoffline;
h$webkit_dom_dom_window_get_onoffline = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onoffline"];
                                         });
var h$webkit_dom_dom_window_set_ononline;
h$webkit_dom_dom_window_set_ononline = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ononline"] = val;
                                        });
var h$webkit_dom_dom_window_get_ononline;
h$webkit_dom_dom_window_get_ononline = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ononline"];
                                        });
var h$webkit_dom_dom_window_set_onpagehide;
h$webkit_dom_dom_window_set_onpagehide = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onpagehide"] = val;
                                          });
var h$webkit_dom_dom_window_get_onpagehide;
h$webkit_dom_dom_window_get_onpagehide = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onpagehide"];
                                          });
var h$webkit_dom_dom_window_set_onpageshow;
h$webkit_dom_dom_window_set_onpageshow = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onpageshow"] = val;
                                          });
var h$webkit_dom_dom_window_get_onpageshow;
h$webkit_dom_dom_window_get_onpageshow = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onpageshow"];
                                          });
var h$webkit_dom_dom_window_set_onpause;
h$webkit_dom_dom_window_set_onpause = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onpause"] = val;
                                       });
var h$webkit_dom_dom_window_get_onpause;
h$webkit_dom_dom_window_get_onpause = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onpause"];
                                       });
var h$webkit_dom_dom_window_set_onplay;
h$webkit_dom_dom_window_set_onplay = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onplay"] = val;
                                      });
var h$webkit_dom_dom_window_get_onplay;
h$webkit_dom_dom_window_get_onplay = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onplay"];
                                      });
var h$webkit_dom_dom_window_set_onplaying;
h$webkit_dom_dom_window_set_onplaying = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onplaying"] = val;
                                         });
var h$webkit_dom_dom_window_get_onplaying;
h$webkit_dom_dom_window_get_onplaying = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onplaying"];
                                         });
var h$webkit_dom_dom_window_set_onpopstate;
h$webkit_dom_dom_window_set_onpopstate = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onpopstate"] = val;
                                          });
var h$webkit_dom_dom_window_get_onpopstate;
h$webkit_dom_dom_window_get_onpopstate = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onpopstate"];
                                          });
var h$webkit_dom_dom_window_set_onprogress;
h$webkit_dom_dom_window_set_onprogress = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onprogress"] = val;
                                          });
var h$webkit_dom_dom_window_get_onprogress;
h$webkit_dom_dom_window_get_onprogress = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onprogress"];
                                          });
var h$webkit_dom_dom_window_set_onratechange;
h$webkit_dom_dom_window_set_onratechange = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["onratechange"] = val;
                                            });
var h$webkit_dom_dom_window_get_onratechange;
h$webkit_dom_dom_window_get_onratechange = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["onratechange"];
                                            });
var h$webkit_dom_dom_window_set_onresize;
h$webkit_dom_dom_window_set_onresize = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onresize"] = val;
                                        });
var h$webkit_dom_dom_window_get_onresize;
h$webkit_dom_dom_window_get_onresize = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onresize"];
                                        });
var h$webkit_dom_dom_window_set_onscroll;
h$webkit_dom_dom_window_set_onscroll = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onscroll"] = val;
                                        });
var h$webkit_dom_dom_window_get_onscroll;
h$webkit_dom_dom_window_get_onscroll = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onscroll"];
                                        });
var h$webkit_dom_dom_window_set_onseeked;
h$webkit_dom_dom_window_set_onseeked = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onseeked"] = val;
                                        });
var h$webkit_dom_dom_window_get_onseeked;
h$webkit_dom_dom_window_get_onseeked = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onseeked"];
                                        });
var h$webkit_dom_dom_window_set_onseeking;
h$webkit_dom_dom_window_set_onseeking = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onseeking"] = val;
                                         });
var h$webkit_dom_dom_window_get_onseeking;
h$webkit_dom_dom_window_get_onseeking = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onseeking"];
                                         });
var h$webkit_dom_dom_window_set_onselect;
h$webkit_dom_dom_window_set_onselect = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onselect"] = val;
                                        });
var h$webkit_dom_dom_window_get_onselect;
h$webkit_dom_dom_window_get_onselect = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onselect"];
                                        });
var h$webkit_dom_dom_window_set_onstalled;
h$webkit_dom_dom_window_set_onstalled = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onstalled"] = val;
                                         });
var h$webkit_dom_dom_window_get_onstalled;
h$webkit_dom_dom_window_get_onstalled = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onstalled"];
                                         });
var h$webkit_dom_dom_window_set_onstorage;
h$webkit_dom_dom_window_set_onstorage = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onstorage"] = val;
                                         });
var h$webkit_dom_dom_window_get_onstorage;
h$webkit_dom_dom_window_get_onstorage = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onstorage"];
                                         });
var h$webkit_dom_dom_window_set_onsubmit;
h$webkit_dom_dom_window_set_onsubmit = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onsubmit"] = val;
                                        });
var h$webkit_dom_dom_window_get_onsubmit;
h$webkit_dom_dom_window_get_onsubmit = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onsubmit"];
                                        });
var h$webkit_dom_dom_window_set_onsuspend;
h$webkit_dom_dom_window_set_onsuspend = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onsuspend"] = val;
                                         });
var h$webkit_dom_dom_window_get_onsuspend;
h$webkit_dom_dom_window_get_onsuspend = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onsuspend"];
                                         });
var h$webkit_dom_dom_window_set_ontimeupdate;
h$webkit_dom_dom_window_set_ontimeupdate = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["ontimeupdate"] = val;
                                            });
var h$webkit_dom_dom_window_get_ontimeupdate;
h$webkit_dom_dom_window_get_ontimeupdate = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["ontimeupdate"];
                                            });
var h$webkit_dom_dom_window_set_onunload;
h$webkit_dom_dom_window_set_onunload = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onunload"] = val;
                                        });
var h$webkit_dom_dom_window_get_onunload;
h$webkit_dom_dom_window_get_onunload = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onunload"];
                                        });
var h$webkit_dom_dom_window_set_onvolumechange;
h$webkit_dom_dom_window_set_onvolumechange = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["onvolumechange"] = val;
                                              });
var h$webkit_dom_dom_window_get_onvolumechange;
h$webkit_dom_dom_window_get_onvolumechange = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["onvolumechange"];
                                              });
var h$webkit_dom_dom_window_set_onwaiting;
h$webkit_dom_dom_window_set_onwaiting = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onwaiting"] = val;
                                         });
var h$webkit_dom_dom_window_get_onwaiting;
h$webkit_dom_dom_window_get_onwaiting = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onwaiting"];
                                         });
var h$webkit_dom_dom_window_set_onreset;
h$webkit_dom_dom_window_set_onreset = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onreset"] = val;
                                       });
var h$webkit_dom_dom_window_get_onreset;
h$webkit_dom_dom_window_get_onreset = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onreset"];
                                       });
var h$webkit_dom_dom_window_set_onsearch;
h$webkit_dom_dom_window_set_onsearch = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onsearch"] = val;
                                        });
var h$webkit_dom_dom_window_get_onsearch;
h$webkit_dom_dom_window_get_onsearch = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onsearch"];
                                        });
var h$webkit_dom_dom_window_set_onwebkitanimationend;
h$webkit_dom_dom_window_set_onwebkitanimationend = (function(self,
                                                    self_2, val, val_2)
                                                    {
                                                      self["onwebkitanimationend"] = val;
                                                    });
var h$webkit_dom_dom_window_get_onwebkitanimationend;
h$webkit_dom_dom_window_get_onwebkitanimationend = (function(self,
                                                    self_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["onwebkitanimationend"];
                                                    });
var h$webkit_dom_dom_window_set_onwebkitanimationiteration;
h$webkit_dom_dom_window_set_onwebkitanimationiteration = (function(self,
                                                          self_2, val, val_2)
                                                          {
                                                            self["onwebkitanimationiteration"] = val;
                                                          });
var h$webkit_dom_dom_window_get_onwebkitanimationiteration;
h$webkit_dom_dom_window_get_onwebkitanimationiteration = (function(self,
                                                          self_2)
                                                          {
                                                            h$ret1 = 0;
                                                            return self["onwebkitanimationiteration"];
                                                          });
var h$webkit_dom_dom_window_set_onwebkitanimationstart;
h$webkit_dom_dom_window_set_onwebkitanimationstart = (function(self,
                                                      self_2, val, val_2)
                                                      {
                                                        self["onwebkitanimationstart"] = val;
                                                      });
var h$webkit_dom_dom_window_get_onwebkitanimationstart;
h$webkit_dom_dom_window_get_onwebkitanimationstart = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["onwebkitanimationstart"];
                                                      });
var h$webkit_dom_dom_window_set_onwebkittransitionend;
h$webkit_dom_dom_window_set_onwebkittransitionend = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onwebkittransitionend"] = val;
                                                     });
var h$webkit_dom_dom_window_get_onwebkittransitionend;
h$webkit_dom_dom_window_get_onwebkittransitionend = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onwebkittransitionend"];
                                                     });
var h$webkit_dom_dom_window_set_ontouchstart;
h$webkit_dom_dom_window_set_ontouchstart = (function(self,
                                            self_2, val, val_2)
                                            {
                                              self["ontouchstart"] = val;
                                            });
var h$webkit_dom_dom_window_get_ontouchstart;
h$webkit_dom_dom_window_get_ontouchstart = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["ontouchstart"];
                                            });
var h$webkit_dom_dom_window_set_ontouchmove;
h$webkit_dom_dom_window_set_ontouchmove = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["ontouchmove"] = val;
                                           });
var h$webkit_dom_dom_window_get_ontouchmove;
h$webkit_dom_dom_window_get_ontouchmove = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["ontouchmove"];
                                           });
var h$webkit_dom_dom_window_set_ontouchend;
h$webkit_dom_dom_window_set_ontouchend = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["ontouchend"] = val;
                                          });
var h$webkit_dom_dom_window_get_ontouchend;
h$webkit_dom_dom_window_get_ontouchend = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["ontouchend"];
                                          });
var h$webkit_dom_dom_window_set_ontouchcancel;
h$webkit_dom_dom_window_set_ontouchcancel = (function(self,
                                             self_2, val, val_2)
                                             {
                                               self["ontouchcancel"] = val;
                                             });
var h$webkit_dom_dom_window_get_ontouchcancel;
h$webkit_dom_dom_window_get_ontouchcancel = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["ontouchcancel"];
                                             });
var h$webkit_dom_dom_window_set_ondevicemotion;
h$webkit_dom_dom_window_set_ondevicemotion = (function(self,
                                              self_2, val, val_2)
                                              {
                                                self["ondevicemotion"] = val;
                                              });
var h$webkit_dom_dom_window_get_ondevicemotion;
h$webkit_dom_dom_window_get_ondevicemotion = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["ondevicemotion"];
                                              });
var h$webkit_dom_dom_window_set_ondeviceorientation;
h$webkit_dom_dom_window_set_ondeviceorientation = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["ondeviceorientation"] = val;
                                                   });
var h$webkit_dom_dom_window_get_ondeviceorientation;
h$webkit_dom_dom_window_get_ondeviceorientation = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["ondeviceorientation"];
                                                   });
var h$webkit_dom_dom_window_set_onwebkitdeviceproximity;
h$webkit_dom_dom_window_set_onwebkitdeviceproximity = (function(self,
                                                       self_2, val, val_2)
                                                       {
                                                         self["onwebkitdeviceproximity"] = val;
                                                       });
var h$webkit_dom_dom_window_get_onwebkitdeviceproximity;
h$webkit_dom_dom_window_get_onwebkitdeviceproximity = (function(self,
                                                       self_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return self["onwebkitdeviceproximity"];
                                                       });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_dom_token_list_get_type = (function()
                                        {
                                          return h$g_get_type(DOMTokenList);
                                        });
var h$webkit_dom_dom_token_list_item;
h$webkit_dom_dom_token_list_item = (function(self,
                                    self_2, index)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["item"](index));
                                    });
var h$webkit_dom_dom_token_list_contains;
h$webkit_dom_dom_token_list_contains = (function(self,
                                        self_2, token, token_2)
                                        {
                                          return self["contains"](h$decodeUtf8z(token,
                                          token_2));
                                        });
var h$webkit_dom_dom_token_list_add;
h$webkit_dom_dom_token_list_add = (function(self,
                                   self_2, token, token_2)
                                   {
                                     return self["add"](h$decodeUtf8z(token,
                                     token_2));
                                   });
var h$webkit_dom_dom_token_list_remove;
h$webkit_dom_dom_token_list_remove = (function(self,
                                      self_2, token, token_2)
                                      {
                                        return self["remove"](h$decodeUtf8z(token,
                                        token_2));
                                      });
var h$webkit_dom_dom_token_list_toggle;
h$webkit_dom_dom_token_list_toggle = (function(self,
                                      self_2, token, token_2)
                                      {
                                        return self["toggle"](h$decodeUtf8z(token,
                                        token_2));
                                      });
var h$webkit_dom_dom_token_list_get_length;
h$webkit_dom_dom_token_list_get_length = (function(self,
                                          self_2)
                                          {
                                            return self["length"];
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_dom_string_list_get_type = (function()
                                         {
                                           return h$g_get_type(DOMStringList);
                                         });
var h$webkit_dom_dom_string_list_item;
h$webkit_dom_dom_string_list_item = (function(self,
                                     self_2, index)
                                     {
                                       h$ret1 = 0;
                                       return h$encodeUtf8(self["item"](index));
                                     });
var h$webkit_dom_dom_string_list_contains;
h$webkit_dom_dom_string_list_contains = (function(self,
                                         self_2, string, string_2)
                                         {
                                           return self["contains"](h$decodeUtf8z(string,
                                           string_2));
                                         });
var h$webkit_dom_dom_string_list_get_length;
h$webkit_dom_dom_string_list_get_length = (function(self,
                                           self_2)
                                           {
                                             return self["length"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_dom_settable_token_list_get_type = (function()
                                                 {
                                                   return h$g_get_type(DOMSettableTokenList);
                                                 });
var h$webkit_dom_dom_settable_token_list_set_value;
h$webkit_dom_dom_settable_token_list_set_value = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["value"] = h$decodeUtf8z(val,
                                                    val_2);
                                                  });
var h$webkit_dom_dom_settable_token_list_get_value;
h$webkit_dom_dom_settable_token_list_get_value = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["value"]);
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_dom_selection_get_type = (function()
                                       {
                                         return h$g_get_type(DOMSelection);
                                       });
var h$webkit_dom_dom_selection_collapse;
h$webkit_dom_dom_selection_collapse = (function(self,
                                       self_2, node, node_2, index)
                                       {
                                         return self["collapse"](node,
                                         index);
                                       });
var h$webkit_dom_dom_selection_collapse_to_end;
h$webkit_dom_dom_selection_collapse_to_end = (function(self,
                                              self_2)
                                              {
                                                return self["collapseToEnd"]();
                                              });
var h$webkit_dom_dom_selection_collapse_to_start;
h$webkit_dom_dom_selection_collapse_to_start = (function(self,
                                                self_2)
                                                {
                                                  return self["collapseToStart"]();
                                                });
var h$webkit_dom_dom_selection_delete_from_document;
h$webkit_dom_dom_selection_delete_from_document = (function(self,
                                                   self_2)
                                                   {
                                                     return self["deleteFromDocument"]();
                                                   });
var h$webkit_dom_dom_selection_contains_node;
h$webkit_dom_dom_selection_contains_node = (function(self,
                                            self_2, node, node_2,
                                            allowPartial)
                                            {
                                              return self["containsNode"](node,
                                              allowPartial);
                                            });
var h$webkit_dom_dom_selection_select_all_children;
h$webkit_dom_dom_selection_select_all_children = (function(self,
                                                  self_2, node, node_2)
                                                  {
                                                    return self["selectAllChildren"](node);
                                                  });
var h$webkit_dom_dom_selection_extend;
h$webkit_dom_dom_selection_extend = (function(self,
                                     self_2, node, node_2, offset)
                                     {
                                       return self["extend"](node,
                                       offset);
                                     });
var h$webkit_dom_dom_selection_get_range_at;
h$webkit_dom_dom_selection_get_range_at = (function(self,
                                           self_2, index)
                                           {
                                             h$ret1 = 0;
                                             return self["getRangeAt"](index);
                                           });
var h$webkit_dom_dom_selection_remove_all_ranges;
h$webkit_dom_dom_selection_remove_all_ranges = (function(self,
                                                self_2)
                                                {
                                                  return self["removeAllRanges"]();
                                                });
var h$webkit_dom_dom_selection_add_range;
h$webkit_dom_dom_selection_add_range = (function(self,
                                        self_2, range, range_2)
                                        {
                                          return self["addRange"](range);
                                        });
var h$webkit_dom_dom_selection_modify;
h$webkit_dom_dom_selection_modify = (function(self,
                                     self_2, alter, alter_2,
                                     direction, direction_2,
                                     granularity, granularity_2)
                                     {
                                       return self["modify"](h$decodeUtf8z(alter,
                                       alter_2),
                                       h$decodeUtf8z(direction,
                                       direction_2),
                                       h$decodeUtf8z(granularity,
                                       granularity_2));
                                     });
var h$webkit_dom_dom_selection_set_base_and_extent;
h$webkit_dom_dom_selection_set_base_and_extent = (function(self,
                                                  self_2, baseNode, baseNode_2,
                                                  baseOffset, extentNode,
                                                  extentNode_2, extentOffset)
                                                  {
                                                    return self["setBaseAndExtent"](baseNode,
                                                    baseOffset, extentNode,
                                                    extentOffset);
                                                  });
var h$webkit_dom_dom_selection_set_position;
h$webkit_dom_dom_selection_set_position = (function(self,
                                           self_2, node, node_2, offset)
                                           {
                                             return self["setPosition"](node,
                                             offset);
                                           });
var h$webkit_dom_dom_selection_empty;
h$webkit_dom_dom_selection_empty = (function(self,
                                    self_2)
                                    {
                                      return self["empty"]();
                                    });
var h$webkit_dom_dom_selection_get_anchor_node;
h$webkit_dom_dom_selection_get_anchor_node = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["anchorNode"];
                                              });
var h$webkit_dom_dom_selection_get_anchor_offset;
h$webkit_dom_dom_selection_get_anchor_offset = (function(self,
                                                self_2)
                                                {
                                                  return self["anchorOffset"];
                                                });
var h$webkit_dom_dom_selection_get_focus_node;
h$webkit_dom_dom_selection_get_focus_node = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["focusNode"];
                                             });
var h$webkit_dom_dom_selection_get_focus_offset;
h$webkit_dom_dom_selection_get_focus_offset = (function(self,
                                               self_2)
                                               {
                                                 return self["focusOffset"];
                                               });
var h$webkit_dom_dom_selection_get_is_collapsed;
h$webkit_dom_dom_selection_get_is_collapsed = (function(self,
                                               self_2)
                                               {
                                                 return self["isCollapsed"];
                                               });
var h$webkit_dom_dom_selection_get_range_count;
h$webkit_dom_dom_selection_get_range_count = (function(self,
                                              self_2)
                                              {
                                                return self["rangeCount"];
                                              });
var h$webkit_dom_dom_selection_get_base_node;
h$webkit_dom_dom_selection_get_base_node = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["baseNode"];
                                            });
var h$webkit_dom_dom_selection_get_base_offset;
h$webkit_dom_dom_selection_get_base_offset = (function(self,
                                              self_2)
                                              {
                                                return self["baseOffset"];
                                              });
var h$webkit_dom_dom_selection_get_extent_node;
h$webkit_dom_dom_selection_get_extent_node = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["extentNode"];
                                              });
var h$webkit_dom_dom_selection_get_extent_offset;
h$webkit_dom_dom_selection_get_extent_offset = (function(self,
                                                self_2)
                                                {
                                                  return self["extentOffset"];
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_dom_security_policy_get_type = (function()
                                             {
                                               return h$g_get_type(DOMSecurityPolicy);
                                             });
var h$webkit_dom_dom_security_policy_is_active;
h$webkit_dom_dom_security_policy_is_active = (function(self,
                                              self_2)
                                              {
                                                return self["isActive"]();
                                              });
var h$webkit_dom_dom_security_policy_allows_connection_to;
h$webkit_dom_dom_security_policy_allows_connection_to = (function(self,
                                                         self_2, url, url_2)
                                                         {
                                                           return self["allowsConnectionTo"](h$decodeUtf8z(url,
                                                           url_2));
                                                         });
var h$webkit_dom_dom_security_policy_allows_font_from;
h$webkit_dom_dom_security_policy_allows_font_from = (function(self,
                                                     self_2, url, url_2)
                                                     {
                                                       return self["allowsFontFrom"](h$decodeUtf8z(url,
                                                       url_2));
                                                     });
var h$webkit_dom_dom_security_policy_allows_form_action;
h$webkit_dom_dom_security_policy_allows_form_action = (function(self,
                                                       self_2, url, url_2)
                                                       {
                                                         return self["allowsFormAction"](h$decodeUtf8z(url,
                                                         url_2));
                                                       });
var h$webkit_dom_dom_security_policy_allows_frame_from;
h$webkit_dom_dom_security_policy_allows_frame_from = (function(self,
                                                      self_2, url, url_2)
                                                      {
                                                        return self["allowsFrameFrom"](h$decodeUtf8z(url,
                                                        url_2));
                                                      });
var h$webkit_dom_dom_security_policy_allows_image_from;
h$webkit_dom_dom_security_policy_allows_image_from = (function(self,
                                                      self_2, url, url_2)
                                                      {
                                                        return self["allowsImageFrom"](h$decodeUtf8z(url,
                                                        url_2));
                                                      });
var h$webkit_dom_dom_security_policy_allows_media_from;
h$webkit_dom_dom_security_policy_allows_media_from = (function(self,
                                                      self_2, url, url_2)
                                                      {
                                                        return self["allowsMediaFrom"](h$decodeUtf8z(url,
                                                        url_2));
                                                      });
var h$webkit_dom_dom_security_policy_allows_object_from;
h$webkit_dom_dom_security_policy_allows_object_from = (function(self,
                                                       self_2, url, url_2)
                                                       {
                                                         return self["allowsObjectFrom"](h$decodeUtf8z(url,
                                                         url_2));
                                                       });
var h$webkit_dom_dom_security_policy_allows_plugin_type;
h$webkit_dom_dom_security_policy_allows_plugin_type = (function(self,
                                                       self_2, type, type_2)
                                                       {
                                                         return self["allowsPluginType"](h$decodeUtf8z(type,
                                                         type_2));
                                                       });
var h$webkit_dom_dom_security_policy_allows_script_from;
h$webkit_dom_dom_security_policy_allows_script_from = (function(self,
                                                       self_2, url, url_2)
                                                       {
                                                         return self["allowsScriptFrom"](h$decodeUtf8z(url,
                                                         url_2));
                                                       });
var h$webkit_dom_dom_security_policy_allows_style_from;
h$webkit_dom_dom_security_policy_allows_style_from = (function(self,
                                                      self_2, url, url_2)
                                                      {
                                                        return self["allowsStyleFrom"](h$decodeUtf8z(url,
                                                        url_2));
                                                      });
var h$webkit_dom_dom_security_policy_allows_eval;
h$webkit_dom_dom_security_policy_allows_eval = (function(self,
                                                self_2)
                                                {
                                                  return self["allowsEval"]();
                                                });
var h$webkit_dom_dom_security_policy_allows_inline_script;
h$webkit_dom_dom_security_policy_allows_inline_script = (function(self,
                                                         self_2)
                                                         {
                                                           return self["allowsInlineScript"]();
                                                         });
var h$webkit_dom_dom_security_policy_allows_inline_style;
h$webkit_dom_dom_security_policy_allows_inline_style = (function(self,
                                                        self_2)
                                                        {
                                                          return self["allowsInlineStyle"]();
                                                        });
var h$webkit_dom_dom_security_policy_get_report_ur_is;
h$webkit_dom_dom_security_policy_get_report_ur_is = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["reportURIs"];
                                                     });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_dom_plugin_array_get_type = (function()
                                          {
                                            return h$g_get_type(DOMPluginArray);
                                          });
var h$webkit_dom_dom_plugin_array_item;
h$webkit_dom_dom_plugin_array_item = (function(self,
                                      self_2, index)
                                      {
                                        h$ret1 = 0;
                                        return self["item"](index);
                                      });
var h$webkit_dom_dom_plugin_array_named_item;
h$webkit_dom_dom_plugin_array_named_item = (function(self,
                                            self_2, name, name_2)
                                            {
                                              h$ret1 = 0;
                                              return self["namedItem"](h$decodeUtf8z(name,
                                              name_2));
                                            });
var h$webkit_dom_dom_plugin_array_refresh;
h$webkit_dom_dom_plugin_array_refresh = (function(self,
                                         self_2, reload)
                                         {
                                           return self["refresh"](reload);
                                         });
var h$webkit_dom_dom_plugin_array_get_length;
h$webkit_dom_dom_plugin_array_get_length = (function(self,
                                            self_2)
                                            {
                                              return self["length"];
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_dom_plugin_get_type = (function()
                                    {
                                      return h$g_get_type(DOMPlugin);
                                    });
var h$webkit_dom_dom_plugin_item;
h$webkit_dom_dom_plugin_item = (function(self,
                                self_2, index)
                                {
                                  h$ret1 = 0;
                                  return self["item"](index);
                                });
var h$webkit_dom_dom_plugin_named_item;
h$webkit_dom_dom_plugin_named_item = (function(self,
                                      self_2, name, name_2)
                                      {
                                        h$ret1 = 0;
                                        return self["namedItem"](h$decodeUtf8z(name,
                                        name_2));
                                      });
var h$webkit_dom_dom_plugin_get_name;
h$webkit_dom_dom_plugin_get_name = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["name"]);
                                    });
var h$webkit_dom_dom_plugin_get_filename;
h$webkit_dom_dom_plugin_get_filename = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return h$encodeUtf8(self["filename"]);
                                        });
var h$webkit_dom_dom_plugin_get_description;
h$webkit_dom_dom_plugin_get_description = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["description"]);
                                           });
var h$webkit_dom_dom_plugin_get_length;
h$webkit_dom_dom_plugin_get_length = (function(self,
                                      self_2)
                                      {
                                        return self["length"];
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_dom_mime_type_array_get_type = (function()
                                             {
                                               return h$g_get_type(DOMMimeTypeArray);
                                             });
var h$webkit_dom_dom_mime_type_array_item;
h$webkit_dom_dom_mime_type_array_item = (function(self,
                                         self_2, index)
                                         {
                                           h$ret1 = 0;
                                           return self["item"](index);
                                         });
var h$webkit_dom_dom_mime_type_array_named_item;
h$webkit_dom_dom_mime_type_array_named_item = (function(self,
                                               self_2, name, name_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["namedItem"](h$decodeUtf8z(name,
                                                 name_2));
                                               });
var h$webkit_dom_dom_mime_type_array_get_length;
h$webkit_dom_dom_mime_type_array_get_length = (function(self,
                                               self_2)
                                               {
                                                 return self["length"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_dom_mime_type_get_type = (function()
                                       {
                                         return h$g_get_type(DOMMimeType);
                                       });
var h$webkit_dom_dom_mime_type_get_suffixes;
h$webkit_dom_dom_mime_type_get_suffixes = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["suffixes"]);
                                           });
var h$webkit_dom_dom_mime_type_get_description;
h$webkit_dom_dom_mime_type_get_description = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["description"]);
                                              });
var h$webkit_dom_dom_mime_type_get_enabled_plugin;
h$webkit_dom_dom_mime_type_get_enabled_plugin = (function(self,
                                                 self_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["enabledPlugin"];
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_dom_implementation_get_type = (function()
                                            {
                                              return h$g_get_type(DOMImplementation);
                                            });
var h$webkit_dom_dom_implementation_has_feature;
h$webkit_dom_dom_implementation_has_feature = (function(self,
                                               self_2, feature, feature_2,
                                               version, version_2)
                                               {
                                                 return self["hasFeature"](h$decodeUtf8z(feature,
                                                 feature_2),
                                                 h$decodeUtf8z(version,
                                                 version_2));
                                               });
var h$webkit_dom_dom_implementation_create_document_type;
h$webkit_dom_dom_implementation_create_document_type = (function(self,
                                                        self_2, qualifiedName,
                                                        qualifiedName_2,
                                                        publicId, publicId_2,
                                                        systemId, systemId_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["createDocumentType"](h$decodeUtf8z(qualifiedName,
                                                          qualifiedName_2),
                                                          h$decodeUtf8z(publicId,
                                                          publicId_2),
                                                          h$decodeUtf8z(systemId,
                                                          systemId_2));
                                                        });
var h$webkit_dom_dom_implementation_create_document;
h$webkit_dom_dom_implementation_create_document = (function(self,
                                                   self_2, namespaceURI,
                                                   namespaceURI_2,
                                                   qualifiedName,
                                                   qualifiedName_2, doctype,
                                                   doctype_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["createDocument"](h$decodeUtf8z(namespaceURI,
                                                     namespaceURI_2),
                                                     h$decodeUtf8z(qualifiedName,
                                                     qualifiedName_2), doctype);
                                                   });
var h$webkit_dom_dom_implementation_create_css_style_sheet;
h$webkit_dom_dom_implementation_create_css_style_sheet = (function(self,
                                                          self_2, title,
                                                          title_2, media,
                                                          media_2)
                                                          {
                                                            h$ret1 = 0;
                                                            return self["createCSSStyleSheet"](h$decodeUtf8z(title,
                                                            title_2),
                                                            h$decodeUtf8z(media,
                                                            media_2));
                                                          });
var h$webkit_dom_dom_implementation_create_html_document;
h$webkit_dom_dom_implementation_create_html_document = (function(self,
                                                        self_2, title, title_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["createHTMLDocument"](h$decodeUtf8z(title,
                                                          title_2));
                                                        });
// Graphics.UI.Gtk.WebKit.DOM.Offline
h$webkit_dom_dom_application_cache_get_type = (function()
                                               {
                                                 return h$g_get_type(DOMApplicationCache);
                                               });
var h$webkit_dom_dom_application_cache_update;
h$webkit_dom_dom_application_cache_update = (function(self,
                                             self_2)
                                             {
                                               return self["update"]();
                                             });
var h$webkit_dom_dom_application_cache_swap_cache;
h$webkit_dom_dom_application_cache_swap_cache = (function(self,
                                                 self_2)
                                                 {
                                                   return self["swapCache"]();
                                                 });
var h$webkit_dom_dom_application_cache_abort;
h$webkit_dom_dom_application_cache_abort = (function(self,
                                            self_2)
                                            {
                                              return self["abort"]();
                                            });
var h$webkit_dom_dom_application_cache_dispatch_event;
h$webkit_dom_dom_application_cache_dispatch_event = (function(self,
                                                     self_2, evt, evt_2)
                                                     {
                                                       return self["dispatchEvent"](evt);
                                                     });
var h$webkit_dom_dom_application_cache_get_status;
h$webkit_dom_dom_application_cache_get_status = (function(self,
                                                 self_2)
                                                 {
                                                   return self["status"];
                                                 });
var h$webkit_dom_dom_application_cache_set_onchecking;
h$webkit_dom_dom_application_cache_set_onchecking = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onchecking"] = val;
                                                     });
var h$webkit_dom_dom_application_cache_get_onchecking;
h$webkit_dom_dom_application_cache_get_onchecking = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onchecking"];
                                                     });
var h$webkit_dom_dom_application_cache_set_onerror;
h$webkit_dom_dom_application_cache_set_onerror = (function(self,
                                                  self_2, val, val_2)
                                                  {
                                                    self["onerror"] = val;
                                                  });
var h$webkit_dom_dom_application_cache_get_onerror;
h$webkit_dom_dom_application_cache_get_onerror = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["onerror"];
                                                  });
var h$webkit_dom_dom_application_cache_set_onnoupdate;
h$webkit_dom_dom_application_cache_set_onnoupdate = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onnoupdate"] = val;
                                                     });
var h$webkit_dom_dom_application_cache_get_onnoupdate;
h$webkit_dom_dom_application_cache_get_onnoupdate = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onnoupdate"];
                                                     });
var h$webkit_dom_dom_application_cache_set_ondownloading;
h$webkit_dom_dom_application_cache_set_ondownloading = (function(self,
                                                        self_2, val, val_2)
                                                        {
                                                          self["ondownloading"] = val;
                                                        });
var h$webkit_dom_dom_application_cache_get_ondownloading;
h$webkit_dom_dom_application_cache_get_ondownloading = (function(self,
                                                        self_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["ondownloading"];
                                                        });
var h$webkit_dom_dom_application_cache_set_onprogress;
h$webkit_dom_dom_application_cache_set_onprogress = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onprogress"] = val;
                                                     });
var h$webkit_dom_dom_application_cache_get_onprogress;
h$webkit_dom_dom_application_cache_get_onprogress = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onprogress"];
                                                     });
var h$webkit_dom_dom_application_cache_set_onupdateready;
h$webkit_dom_dom_application_cache_set_onupdateready = (function(self,
                                                        self_2, val, val_2)
                                                        {
                                                          self["onupdateready"] = val;
                                                        });
var h$webkit_dom_dom_application_cache_get_onupdateready;
h$webkit_dom_dom_application_cache_get_onupdateready = (function(self,
                                                        self_2)
                                                        {
                                                          h$ret1 = 0;
                                                          return self["onupdateready"];
                                                        });
var h$webkit_dom_dom_application_cache_set_oncached;
h$webkit_dom_dom_application_cache_set_oncached = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["oncached"] = val;
                                                   });
var h$webkit_dom_dom_application_cache_get_oncached;
h$webkit_dom_dom_application_cache_get_oncached = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return self["oncached"];
                                                   });
var h$webkit_dom_dom_application_cache_set_onobsolete;
h$webkit_dom_dom_application_cache_set_onobsolete = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onobsolete"] = val;
                                                     });
var h$webkit_dom_dom_application_cache_get_onobsolete;
h$webkit_dom_dom_application_cache_get_onobsolete = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onobsolete"];
                                                     });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_document_type_get_type = (function()
                                       {
                                         return h$g_get_type(DocumentType);
                                       });
var h$webkit_dom_document_type_get_name;
h$webkit_dom_document_type_get_name = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["name"]);
                                       });
var h$webkit_dom_document_type_get_entities;
h$webkit_dom_document_type_get_entities = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["entities"];
                                           });
var h$webkit_dom_document_type_get_notations;
h$webkit_dom_document_type_get_notations = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["notations"];
                                            });
var h$webkit_dom_document_type_get_public_id;
h$webkit_dom_document_type_get_public_id = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["publicId"]);
                                            });
var h$webkit_dom_document_type_get_system_id;
h$webkit_dom_document_type_get_system_id = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["systemId"]);
                                            });
var h$webkit_dom_document_type_get_internal_subset;
h$webkit_dom_document_type_get_internal_subset = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return h$encodeUtf8(self["internalSubset"]);
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_document_fragment_get_type = (function()
                                           {
                                             return h$g_get_type(DocumentFragment);
                                           });
var h$webkit_dom_document_fragment_query_selector;
h$webkit_dom_document_fragment_query_selector = (function(self,
                                                 self_2, selectors, selectors_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["querySelector"](h$decodeUtf8z(selectors,
                                                   selectors_2));
                                                 });
var h$webkit_dom_document_fragment_query_selector_all;
h$webkit_dom_document_fragment_query_selector_all = (function(self,
                                                     self_2, selectors,
                                                     selectors_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["querySelectorAll"](h$decodeUtf8z(selectors,
                                                       selectors_2));
                                                     });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_document_get_type = (function()
                                  {
                                    return h$g_get_type(Document);
                                  });
var h$webkit_dom_document_create_element;
h$webkit_dom_document_create_element = (function(self,
                                        self_2, tagName, tagName_2)
                                        {
                                          h$ret1 = 0;
                                          return self["createElement"](h$decodeUtf8z(tagName,
                                          tagName_2));
                                        });
var h$webkit_dom_document_create_document_fragment;
h$webkit_dom_document_create_document_fragment = (function(self,
                                                  self_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["createDocumentFragment"]();
                                                  });
var h$webkit_dom_document_create_text_node;
h$webkit_dom_document_create_text_node = (function(self,
                                          self_2, data, data_2)
                                          {
                                            h$ret1 = 0;
                                            return self["createTextNode"](h$decodeUtf8z(data,
                                            data_2));
                                          });
var h$webkit_dom_document_create_comment;
h$webkit_dom_document_create_comment = (function(self,
                                        self_2, data, data_2)
                                        {
                                          h$ret1 = 0;
                                          return self["createComment"](h$decodeUtf8z(data,
                                          data_2));
                                        });
var h$webkit_dom_document_create_cdata_section;
h$webkit_dom_document_create_cdata_section = (function(self,
                                              self_2, data, data_2)
                                              {
                                                h$ret1 = 0;
                                                return self["createCDATASection"](h$decodeUtf8z(data,
                                                data_2));
                                              });
var h$webkit_dom_document_create_processing_instruction;
h$webkit_dom_document_create_processing_instruction = (function(self,
                                                       self_2, target, target_2,
                                                       data, data_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return self["createProcessingInstruction"](h$decodeUtf8z(target,
                                                         target_2),
                                                         h$decodeUtf8z(data,
                                                         data_2));
                                                       });
var h$webkit_dom_document_create_attribute;
h$webkit_dom_document_create_attribute = (function(self,
                                          self_2, name, name_2)
                                          {
                                            h$ret1 = 0;
                                            return self["createAttribute"](h$decodeUtf8z(name,
                                            name_2));
                                          });
var h$webkit_dom_document_create_entity_reference;
h$webkit_dom_document_create_entity_reference = (function(self,
                                                 self_2, name, name_2)
                                                 {
                                                   h$ret1 = 0;
                                                   return self["createEntityReference"](h$decodeUtf8z(name,
                                                   name_2));
                                                 });
var h$webkit_dom_document_get_elements_by_tag_name;
h$webkit_dom_document_get_elements_by_tag_name = (function(self,
                                                  self_2, tagname, tagname_2)
                                                  {
                                                    h$ret1 = 0;
                                                    return self["getElementsByTagName"](h$decodeUtf8z(tagname,
                                                    tagname_2));
                                                  });
var h$webkit_dom_document_import_node;
h$webkit_dom_document_import_node = (function(self,
                                     self_2, importedNode,
                                     importedNode_2, deep)
                                     {
                                       h$ret1 = 0;
                                       return self["importNode"](importedNode,
                                       deep);
                                     });
var h$webkit_dom_document_create_element_ns;
h$webkit_dom_document_create_element_ns = (function(self,
                                           self_2, namespaceURI,
                                           namespaceURI_2, qualifiedName,
                                           qualifiedName_2)
                                           {
                                             h$ret1 = 0;
                                             return self["createElementNS"](h$decodeUtf8z(namespaceURI,
                                             namespaceURI_2),
                                             h$decodeUtf8z(qualifiedName,
                                             qualifiedName_2));
                                           });
var h$webkit_dom_document_create_attribute_ns;
h$webkit_dom_document_create_attribute_ns = (function(self,
                                             self_2, namespaceURI,
                                             namespaceURI_2, qualifiedName,
                                             qualifiedName_2)
                                             {
                                               h$ret1 = 0;
                                               return self["createAttributeNS"](h$decodeUtf8z(namespaceURI,
                                               namespaceURI_2),
                                               h$decodeUtf8z(qualifiedName,
                                               qualifiedName_2));
                                             });
var h$webkit_dom_document_get_elements_by_tag_name_ns;
h$webkit_dom_document_get_elements_by_tag_name_ns = (function(self,
                                                     self_2, namespaceURI,
                                                     namespaceURI_2, localName,
                                                     localName_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["getElementsByTagNameNS"](h$decodeUtf8z(namespaceURI,
                                                       namespaceURI_2),
                                                       h$decodeUtf8z(localName,
                                                       localName_2));
                                                     });
var h$webkit_dom_document_get_element_by_id;
h$webkit_dom_document_get_element_by_id = (function(self,
                                           self_2, elementId, elementId_2)
                                           {
                                             h$ret1 = 0;
                                             return self["getElementById"](h$decodeUtf8z(elementId,
                                             elementId_2));
                                           });
var h$webkit_dom_document_adopt_node;
h$webkit_dom_document_adopt_node = (function(self,
                                    self_2, source, source_2)
                                    {
                                      h$ret1 = 0;
                                      return self["adoptNode"](source);
                                    });
var h$webkit_dom_document_create_event;
h$webkit_dom_document_create_event = (function(self,
                                      self_2, eventType, eventType_2)
                                      {
                                        h$ret1 = 0;
                                        return self["createEvent"](h$decodeUtf8z(eventType,
                                        eventType_2));
                                      });
var h$webkit_dom_document_create_range;
h$webkit_dom_document_create_range = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["createRange"]();
                                      });
var h$webkit_dom_document_create_node_iterator;
h$webkit_dom_document_create_node_iterator = (function(self,
                                              self_2, root, root_2,
                                              whatToShow, filter, filter_2,
                                              expandEntityReferences)
                                              {
                                                h$ret1 = 0;
                                                return self["createNodeIterator"](root,
                                                whatToShow, filter,
                                                expandEntityReferences);
                                              });
var h$webkit_dom_document_create_tree_walker;
h$webkit_dom_document_create_tree_walker = (function(self,
                                            self_2, root, root_2,
                                            whatToShow, filter, filter_2,
                                            expandEntityReferences)
                                            {
                                              h$ret1 = 0;
                                              return self["createTreeWalker"](root,
                                              whatToShow, filter,
                                              expandEntityReferences);
                                            });
var h$webkit_dom_document_get_override_style;
h$webkit_dom_document_get_override_style = (function(self,
                                            self_2, element, element_2,
                                            pseudoElement, pseudoElement_2)
                                            {
                                              h$ret1 = 0;
                                              return self["getOverrideStyle"](element,
                                              h$decodeUtf8z(pseudoElement,
                                              pseudoElement_2));
                                            });
var h$webkit_dom_document_create_expression;
h$webkit_dom_document_create_expression = (function(self,
                                           self_2, expression,
                                           expression_2, resolver,
                                           resolver_2)
                                           {
                                             h$ret1 = 0;
                                             return self["createExpression"](h$decodeUtf8z(expression,
                                             expression_2), resolver);
                                           });
var h$webkit_dom_document_create_ns_resolver;
h$webkit_dom_document_create_ns_resolver = (function(self,
                                            self_2, nodeResolver,
                                            nodeResolver_2)
                                            {
                                              h$ret1 = 0;
                                              return self["createNSResolver"](nodeResolver);
                                            });
var h$webkit_dom_document_evaluate;
h$webkit_dom_document_evaluate = (function(self,
                                  self_2, expression,
                                  expression_2, contextNode,
                                  contextNode_2, resolver,
                                  resolver_2, type, inResult,
                                  inResult_2)
                                  {
                                    h$ret1 = 0;
                                    return self["evaluate"](h$decodeUtf8z(expression,
                                    expression_2), contextNode,
                                    resolver, type, inResult);
                                  });
var h$webkit_dom_document_exec_command;
h$webkit_dom_document_exec_command = (function(self,
                                      self_2, command, command_2,
                                      userInterface, value, value_2)
                                      {
                                        return self["execCommand"](h$decodeUtf8z(command,
                                        command_2), userInterface,
                                        h$decodeUtf8z(value, value_2));
                                      });
var h$webkit_dom_document_query_command_enabled;
h$webkit_dom_document_query_command_enabled = (function(self,
                                               self_2, command, command_2)
                                               {
                                                 return self["queryCommandEnabled"](h$decodeUtf8z(command,
                                                 command_2));
                                               });
var h$webkit_dom_document_query_command_indeterm;
h$webkit_dom_document_query_command_indeterm = (function(self,
                                                self_2, command, command_2)
                                                {
                                                  return self["queryCommandIndeterm"](h$decodeUtf8z(command,
                                                  command_2));
                                                });
var h$webkit_dom_document_query_command_state;
h$webkit_dom_document_query_command_state = (function(self,
                                             self_2, command, command_2)
                                             {
                                               return self["queryCommandState"](h$decodeUtf8z(command,
                                               command_2));
                                             });
var h$webkit_dom_document_query_command_supported;
h$webkit_dom_document_query_command_supported = (function(self,
                                                 self_2, command, command_2)
                                                 {
                                                   return self["queryCommandSupported"](h$decodeUtf8z(command,
                                                   command_2));
                                                 });
var h$webkit_dom_document_query_command_value;
h$webkit_dom_document_query_command_value = (function(self,
                                             self_2, command, command_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["queryCommandValue"](h$decodeUtf8z(command,
                                               command_2)));
                                             });
var h$webkit_dom_document_get_elements_by_name;
h$webkit_dom_document_get_elements_by_name = (function(self,
                                              self_2, elementName,
                                              elementName_2)
                                              {
                                                h$ret1 = 0;
                                                return self["getElementsByName"](h$decodeUtf8z(elementName,
                                                elementName_2));
                                              });
var h$webkit_dom_document_element_from_point;
h$webkit_dom_document_element_from_point = (function(self,
                                            self_2, x, y)
                                            {
                                              h$ret1 = 0;
                                              return self["elementFromPoint"](x,
                                              y);
                                            });
var h$webkit_dom_document_caret_range_from_point;
h$webkit_dom_document_caret_range_from_point = (function(self,
                                                self_2, x, y)
                                                {
                                                  h$ret1 = 0;
                                                  return self["caretRangeFromPoint"](x,
                                                  y);
                                                });
var h$webkit_dom_document_create_css_style_declaration;
h$webkit_dom_document_create_css_style_declaration = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["createCSSStyleDeclaration"]();
                                                      });
var h$webkit_dom_document_get_elements_by_class_name;
h$webkit_dom_document_get_elements_by_class_name = (function(self,
                                                    self_2, tagname, tagname_2)
                                                    {
                                                      h$ret1 = 0;
                                                      return self["getElementsByClassName"](h$decodeUtf8z(tagname,
                                                      tagname_2));
                                                    });
var h$webkit_dom_document_query_selector;
h$webkit_dom_document_query_selector = (function(self,
                                        self_2, selectors, selectors_2)
                                        {
                                          h$ret1 = 0;
                                          return self["querySelector"](h$decodeUtf8z(selectors,
                                          selectors_2));
                                        });
var h$webkit_dom_document_query_selector_all;
h$webkit_dom_document_query_selector_all = (function(self,
                                            self_2, selectors, selectors_2)
                                            {
                                              h$ret1 = 0;
                                              return self["querySelectorAll"](h$decodeUtf8z(selectors,
                                              selectors_2));
                                            });
var h$webkit_dom_document_get_doctype;
h$webkit_dom_document_get_doctype = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["doctype"];
                                     });
var h$webkit_dom_document_get_implementation;
h$webkit_dom_document_get_implementation = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return self["implementation"];
                                            });
var h$webkit_dom_document_get_document_element;
h$webkit_dom_document_get_document_element = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["documentElement"];
                                              });
var h$webkit_dom_document_get_input_encoding;
h$webkit_dom_document_get_input_encoding = (function(self,
                                            self_2)
                                            {
                                              h$ret1 = 0;
                                              return h$encodeUtf8(self["inputEncoding"]);
                                            });
var h$webkit_dom_document_get_xml_encoding;
h$webkit_dom_document_get_xml_encoding = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["xmlEncoding"]);
                                          });
var h$webkit_dom_document_set_xml_version;
h$webkit_dom_document_set_xml_version = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["xmlVersion"] = h$decodeUtf8z(val,
                                           val_2);
                                         });
var h$webkit_dom_document_get_xml_version;
h$webkit_dom_document_get_xml_version = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["xmlVersion"]);
                                         });
var h$webkit_dom_document_set_xml_standalone;
h$webkit_dom_document_set_xml_standalone = (function(self,
                                            self_2, val)
                                            {
                                              self["xmlStandalone"] = val;
                                            });
var h$webkit_dom_document_get_xml_standalone;
h$webkit_dom_document_get_xml_standalone = (function(self,
                                            self_2)
                                            {
                                              return self["xmlStandalone"];
                                            });
var h$webkit_dom_document_set_document_uri;
h$webkit_dom_document_set_document_uri = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["documentURI"] = h$decodeUtf8z(val,
                                            val_2);
                                          });
var h$webkit_dom_document_get_document_uri;
h$webkit_dom_document_get_document_uri = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return h$encodeUtf8(self["documentURI"]);
                                          });
var h$webkit_dom_document_get_default_view;
h$webkit_dom_document_get_default_view = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["defaultView"];
                                          });
var h$webkit_dom_document_get_style_sheets;
h$webkit_dom_document_get_style_sheets = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["styleSheets"];
                                          });
var h$webkit_dom_document_set_title;
h$webkit_dom_document_set_title = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["title"] = h$decodeUtf8z(val,
                                     val_2);
                                   });
var h$webkit_dom_document_get_title;
h$webkit_dom_document_get_title = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return h$encodeUtf8(self["title"]);
                                   });
var h$webkit_dom_document_get_referrer;
h$webkit_dom_document_get_referrer = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["referrer"]);
                                      });
var h$webkit_dom_document_get_domain;
h$webkit_dom_document_get_domain = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["domain"]);
                                    });
var h$webkit_dom_document_set_cookie;
h$webkit_dom_document_set_cookie = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["cookie"] = h$decodeUtf8z(val,
                                      val_2);
                                    });
var h$webkit_dom_document_get_cookie;
h$webkit_dom_document_get_cookie = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return h$encodeUtf8(self["cookie"]);
                                    });
var h$webkit_dom_document_set_body;
h$webkit_dom_document_set_body = (function(self,
                                  self_2, val, val_2)
                                  {
                                    self["body"] = val;
                                  });
var h$webkit_dom_document_get_body;
h$webkit_dom_document_get_body = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return self["body"];
                                  });
var h$webkit_dom_document_get_head;
h$webkit_dom_document_get_head = (function(self,
                                  self_2)
                                  {
                                    h$ret1 = 0;
                                    return self["head"];
                                  });
var h$webkit_dom_document_get_images;
h$webkit_dom_document_get_images = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["images"];
                                    });
var h$webkit_dom_document_get_applets;
h$webkit_dom_document_get_applets = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["applets"];
                                     });
var h$webkit_dom_document_get_links;
h$webkit_dom_document_get_links = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["links"];
                                   });
var h$webkit_dom_document_get_forms;
h$webkit_dom_document_get_forms = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["forms"];
                                   });
var h$webkit_dom_document_get_anchors;
h$webkit_dom_document_get_anchors = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["anchors"];
                                     });
var h$webkit_dom_document_get_last_modified;
h$webkit_dom_document_get_last_modified = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["lastModified"]);
                                           });
var h$webkit_dom_document_set_charset;
h$webkit_dom_document_set_charset = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["charset"] = h$decodeUtf8z(val,
                                       val_2);
                                     });
var h$webkit_dom_document_get_charset;
h$webkit_dom_document_get_charset = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return h$encodeUtf8(self["charset"]);
                                     });
var h$webkit_dom_document_get_default_charset;
h$webkit_dom_document_get_default_charset = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return h$encodeUtf8(self["defaultCharset"]);
                                             });
var h$webkit_dom_document_get_ready_state;
h$webkit_dom_document_get_ready_state = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["readyState"]);
                                         });
var h$webkit_dom_document_get_character_set;
h$webkit_dom_document_get_character_set = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["characterSet"]);
                                           });
var h$webkit_dom_document_get_preferred_stylesheet_set;
h$webkit_dom_document_get_preferred_stylesheet_set = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return h$encodeUtf8(self["preferredStylesheetSet"]);
                                                      });
var h$webkit_dom_document_set_selected_stylesheet_set;
h$webkit_dom_document_set_selected_stylesheet_set = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["selectedStylesheetSet"] = h$decodeUtf8z(val,
                                                       val_2);
                                                     });
var h$webkit_dom_document_get_selected_stylesheet_set;
h$webkit_dom_document_get_selected_stylesheet_set = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["selectedStylesheetSet"]);
                                                     });
var h$webkit_dom_document_get_compat_mode;
h$webkit_dom_document_get_compat_mode = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return h$encodeUtf8(self["compatMode"]);
                                         });
var h$webkit_dom_document_get_webkit_pointer_lock_element;
h$webkit_dom_document_get_webkit_pointer_lock_element = (function(self,
                                                         self_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return self["webkitPointerLockElement"];
                                                         });
var h$webkit_dom_document_set_onabort;
h$webkit_dom_document_set_onabort = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onabort"] = val;
                                     });
var h$webkit_dom_document_get_onabort;
h$webkit_dom_document_get_onabort = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onabort"];
                                     });
var h$webkit_dom_document_set_onblur;
h$webkit_dom_document_set_onblur = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onblur"] = val;
                                    });
var h$webkit_dom_document_get_onblur;
h$webkit_dom_document_get_onblur = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onblur"];
                                    });
var h$webkit_dom_document_set_onchange;
h$webkit_dom_document_set_onchange = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onchange"] = val;
                                      });
var h$webkit_dom_document_get_onchange;
h$webkit_dom_document_get_onchange = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onchange"];
                                      });
var h$webkit_dom_document_set_onclick;
h$webkit_dom_document_set_onclick = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onclick"] = val;
                                     });
var h$webkit_dom_document_get_onclick;
h$webkit_dom_document_get_onclick = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onclick"];
                                     });
var h$webkit_dom_document_set_oncontextmenu;
h$webkit_dom_document_set_oncontextmenu = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["oncontextmenu"] = val;
                                           });
var h$webkit_dom_document_get_oncontextmenu;
h$webkit_dom_document_get_oncontextmenu = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["oncontextmenu"];
                                           });
var h$webkit_dom_document_set_ondblclick;
h$webkit_dom_document_set_ondblclick = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ondblclick"] = val;
                                        });
var h$webkit_dom_document_get_ondblclick;
h$webkit_dom_document_get_ondblclick = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ondblclick"];
                                        });
var h$webkit_dom_document_set_ondrag;
h$webkit_dom_document_set_ondrag = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["ondrag"] = val;
                                    });
var h$webkit_dom_document_get_ondrag;
h$webkit_dom_document_get_ondrag = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["ondrag"];
                                    });
var h$webkit_dom_document_set_ondragend;
h$webkit_dom_document_set_ondragend = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["ondragend"] = val;
                                       });
var h$webkit_dom_document_get_ondragend;
h$webkit_dom_document_get_ondragend = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["ondragend"];
                                       });
var h$webkit_dom_document_set_ondragenter;
h$webkit_dom_document_set_ondragenter = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["ondragenter"] = val;
                                         });
var h$webkit_dom_document_get_ondragenter;
h$webkit_dom_document_get_ondragenter = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["ondragenter"];
                                         });
var h$webkit_dom_document_set_ondragleave;
h$webkit_dom_document_set_ondragleave = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["ondragleave"] = val;
                                         });
var h$webkit_dom_document_get_ondragleave;
h$webkit_dom_document_get_ondragleave = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["ondragleave"];
                                         });
var h$webkit_dom_document_set_ondragover;
h$webkit_dom_document_set_ondragover = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ondragover"] = val;
                                        });
var h$webkit_dom_document_get_ondragover;
h$webkit_dom_document_get_ondragover = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ondragover"];
                                        });
var h$webkit_dom_document_set_ondragstart;
h$webkit_dom_document_set_ondragstart = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["ondragstart"] = val;
                                         });
var h$webkit_dom_document_get_ondragstart;
h$webkit_dom_document_get_ondragstart = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["ondragstart"];
                                         });
var h$webkit_dom_document_set_ondrop;
h$webkit_dom_document_set_ondrop = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["ondrop"] = val;
                                    });
var h$webkit_dom_document_get_ondrop;
h$webkit_dom_document_get_ondrop = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["ondrop"];
                                    });
var h$webkit_dom_document_set_onerror;
h$webkit_dom_document_set_onerror = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onerror"] = val;
                                     });
var h$webkit_dom_document_get_onerror;
h$webkit_dom_document_get_onerror = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onerror"];
                                     });
var h$webkit_dom_document_set_onfocus;
h$webkit_dom_document_set_onfocus = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onfocus"] = val;
                                     });
var h$webkit_dom_document_get_onfocus;
h$webkit_dom_document_get_onfocus = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onfocus"];
                                     });
var h$webkit_dom_document_set_oninput;
h$webkit_dom_document_set_oninput = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["oninput"] = val;
                                     });
var h$webkit_dom_document_get_oninput;
h$webkit_dom_document_get_oninput = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["oninput"];
                                     });
var h$webkit_dom_document_set_oninvalid;
h$webkit_dom_document_set_oninvalid = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["oninvalid"] = val;
                                       });
var h$webkit_dom_document_get_oninvalid;
h$webkit_dom_document_get_oninvalid = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["oninvalid"];
                                       });
var h$webkit_dom_document_set_onkeydown;
h$webkit_dom_document_set_onkeydown = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onkeydown"] = val;
                                       });
var h$webkit_dom_document_get_onkeydown;
h$webkit_dom_document_get_onkeydown = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onkeydown"];
                                       });
var h$webkit_dom_document_set_onkeypress;
h$webkit_dom_document_set_onkeypress = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onkeypress"] = val;
                                        });
var h$webkit_dom_document_get_onkeypress;
h$webkit_dom_document_get_onkeypress = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onkeypress"];
                                        });
var h$webkit_dom_document_set_onkeyup;
h$webkit_dom_document_set_onkeyup = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onkeyup"] = val;
                                     });
var h$webkit_dom_document_get_onkeyup;
h$webkit_dom_document_get_onkeyup = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onkeyup"];
                                     });
var h$webkit_dom_document_set_onload;
h$webkit_dom_document_set_onload = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["onload"] = val;
                                    });
var h$webkit_dom_document_get_onload;
h$webkit_dom_document_get_onload = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["onload"];
                                    });
var h$webkit_dom_document_set_onmousedown;
h$webkit_dom_document_set_onmousedown = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onmousedown"] = val;
                                         });
var h$webkit_dom_document_get_onmousedown;
h$webkit_dom_document_get_onmousedown = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onmousedown"];
                                         });
var h$webkit_dom_document_set_onmousemove;
h$webkit_dom_document_set_onmousemove = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onmousemove"] = val;
                                         });
var h$webkit_dom_document_get_onmousemove;
h$webkit_dom_document_get_onmousemove = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onmousemove"];
                                         });
var h$webkit_dom_document_set_onmouseout;
h$webkit_dom_document_set_onmouseout = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["onmouseout"] = val;
                                        });
var h$webkit_dom_document_get_onmouseout;
h$webkit_dom_document_get_onmouseout = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["onmouseout"];
                                        });
var h$webkit_dom_document_set_onmouseover;
h$webkit_dom_document_set_onmouseover = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onmouseover"] = val;
                                         });
var h$webkit_dom_document_get_onmouseover;
h$webkit_dom_document_get_onmouseover = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onmouseover"];
                                         });
var h$webkit_dom_document_set_onmouseup;
h$webkit_dom_document_set_onmouseup = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["onmouseup"] = val;
                                       });
var h$webkit_dom_document_get_onmouseup;
h$webkit_dom_document_get_onmouseup = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["onmouseup"];
                                       });
var h$webkit_dom_document_set_onmousewheel;
h$webkit_dom_document_set_onmousewheel = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onmousewheel"] = val;
                                          });
var h$webkit_dom_document_get_onmousewheel;
h$webkit_dom_document_get_onmousewheel = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onmousewheel"];
                                          });
var h$webkit_dom_document_set_onreadystatechange;
h$webkit_dom_document_set_onreadystatechange = (function(self,
                                                self_2, val, val_2)
                                                {
                                                  self["onreadystatechange"] = val;
                                                });
var h$webkit_dom_document_get_onreadystatechange;
h$webkit_dom_document_get_onreadystatechange = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["onreadystatechange"];
                                                });
var h$webkit_dom_document_set_onscroll;
h$webkit_dom_document_set_onscroll = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onscroll"] = val;
                                      });
var h$webkit_dom_document_get_onscroll;
h$webkit_dom_document_get_onscroll = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onscroll"];
                                      });
var h$webkit_dom_document_set_onselect;
h$webkit_dom_document_set_onselect = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onselect"] = val;
                                      });
var h$webkit_dom_document_get_onselect;
h$webkit_dom_document_get_onselect = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onselect"];
                                      });
var h$webkit_dom_document_set_onsubmit;
h$webkit_dom_document_set_onsubmit = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onsubmit"] = val;
                                      });
var h$webkit_dom_document_get_onsubmit;
h$webkit_dom_document_get_onsubmit = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onsubmit"];
                                      });
var h$webkit_dom_document_set_onbeforecut;
h$webkit_dom_document_set_onbeforecut = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["onbeforecut"] = val;
                                         });
var h$webkit_dom_document_get_onbeforecut;
h$webkit_dom_document_get_onbeforecut = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["onbeforecut"];
                                         });
var h$webkit_dom_document_set_oncut;
h$webkit_dom_document_set_oncut = (function(self,
                                   self_2, val, val_2)
                                   {
                                     self["oncut"] = val;
                                   });
var h$webkit_dom_document_get_oncut;
h$webkit_dom_document_get_oncut = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["oncut"];
                                   });
var h$webkit_dom_document_set_onbeforecopy;
h$webkit_dom_document_set_onbeforecopy = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["onbeforecopy"] = val;
                                          });
var h$webkit_dom_document_get_onbeforecopy;
h$webkit_dom_document_get_onbeforecopy = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["onbeforecopy"];
                                          });
var h$webkit_dom_document_set_oncopy;
h$webkit_dom_document_set_oncopy = (function(self,
                                    self_2, val, val_2)
                                    {
                                      self["oncopy"] = val;
                                    });
var h$webkit_dom_document_get_oncopy;
h$webkit_dom_document_get_oncopy = (function(self,
                                    self_2)
                                    {
                                      h$ret1 = 0;
                                      return self["oncopy"];
                                    });
var h$webkit_dom_document_set_onbeforepaste;
h$webkit_dom_document_set_onbeforepaste = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["onbeforepaste"] = val;
                                           });
var h$webkit_dom_document_get_onbeforepaste;
h$webkit_dom_document_get_onbeforepaste = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["onbeforepaste"];
                                           });
var h$webkit_dom_document_set_onpaste;
h$webkit_dom_document_set_onpaste = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onpaste"] = val;
                                     });
var h$webkit_dom_document_get_onpaste;
h$webkit_dom_document_get_onpaste = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onpaste"];
                                     });
var h$webkit_dom_document_set_onreset;
h$webkit_dom_document_set_onreset = (function(self,
                                     self_2, val, val_2)
                                     {
                                       self["onreset"] = val;
                                     });
var h$webkit_dom_document_get_onreset;
h$webkit_dom_document_get_onreset = (function(self,
                                     self_2)
                                     {
                                       h$ret1 = 0;
                                       return self["onreset"];
                                     });
var h$webkit_dom_document_set_onsearch;
h$webkit_dom_document_set_onsearch = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["onsearch"] = val;
                                      });
var h$webkit_dom_document_get_onsearch;
h$webkit_dom_document_get_onsearch = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return self["onsearch"];
                                      });
var h$webkit_dom_document_set_onselectstart;
h$webkit_dom_document_set_onselectstart = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["onselectstart"] = val;
                                           });
var h$webkit_dom_document_get_onselectstart;
h$webkit_dom_document_get_onselectstart = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["onselectstart"];
                                           });
var h$webkit_dom_document_set_onselectionchange;
h$webkit_dom_document_set_onselectionchange = (function(self,
                                               self_2, val, val_2)
                                               {
                                                 self["onselectionchange"] = val;
                                               });
var h$webkit_dom_document_get_onselectionchange;
h$webkit_dom_document_get_onselectionchange = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["onselectionchange"];
                                               });
var h$webkit_dom_document_set_ontouchstart;
h$webkit_dom_document_set_ontouchstart = (function(self,
                                          self_2, val, val_2)
                                          {
                                            self["ontouchstart"] = val;
                                          });
var h$webkit_dom_document_get_ontouchstart;
h$webkit_dom_document_get_ontouchstart = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["ontouchstart"];
                                          });
var h$webkit_dom_document_set_ontouchmove;
h$webkit_dom_document_set_ontouchmove = (function(self,
                                         self_2, val, val_2)
                                         {
                                           self["ontouchmove"] = val;
                                         });
var h$webkit_dom_document_get_ontouchmove;
h$webkit_dom_document_get_ontouchmove = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["ontouchmove"];
                                         });
var h$webkit_dom_document_set_ontouchend;
h$webkit_dom_document_set_ontouchend = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["ontouchend"] = val;
                                        });
var h$webkit_dom_document_get_ontouchend;
h$webkit_dom_document_get_ontouchend = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return self["ontouchend"];
                                        });
var h$webkit_dom_document_set_ontouchcancel;
h$webkit_dom_document_set_ontouchcancel = (function(self,
                                           self_2, val, val_2)
                                           {
                                             self["ontouchcancel"] = val;
                                           });
var h$webkit_dom_document_get_ontouchcancel;
h$webkit_dom_document_get_ontouchcancel = (function(self,
                                           self_2)
                                           {
                                             h$ret1 = 0;
                                             return self["ontouchcancel"];
                                           });
var h$webkit_dom_document_set_onwebkitfullscreenchange;
h$webkit_dom_document_set_onwebkitfullscreenchange = (function(self,
                                                      self_2, val, val_2)
                                                      {
                                                        self["onwebkitfullscreenchange"] = val;
                                                      });
var h$webkit_dom_document_get_onwebkitfullscreenchange;
h$webkit_dom_document_get_onwebkitfullscreenchange = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["onwebkitfullscreenchange"];
                                                      });
var h$webkit_dom_document_set_onwebkitfullscreenerror;
h$webkit_dom_document_set_onwebkitfullscreenerror = (function(self,
                                                     self_2, val, val_2)
                                                     {
                                                       self["onwebkitfullscreenerror"] = val;
                                                     });
var h$webkit_dom_document_get_onwebkitfullscreenerror;
h$webkit_dom_document_get_onwebkitfullscreenerror = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return self["onwebkitfullscreenerror"];
                                                     });
var h$webkit_dom_document_set_onwebkitpointerlockchange;
h$webkit_dom_document_set_onwebkitpointerlockchange = (function(self,
                                                       self_2, val, val_2)
                                                       {
                                                         self["onwebkitpointerlockchange"] = val;
                                                       });
var h$webkit_dom_document_get_onwebkitpointerlockchange;
h$webkit_dom_document_get_onwebkitpointerlockchange = (function(self,
                                                       self_2)
                                                       {
                                                         h$ret1 = 0;
                                                         return self["onwebkitpointerlockchange"];
                                                       });
var h$webkit_dom_document_set_onwebkitpointerlockerror;
h$webkit_dom_document_set_onwebkitpointerlockerror = (function(self,
                                                      self_2, val, val_2)
                                                      {
                                                        self["onwebkitpointerlockerror"] = val;
                                                      });
var h$webkit_dom_document_get_onwebkitpointerlockerror;
h$webkit_dom_document_get_onwebkitpointerlockerror = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["onwebkitpointerlockerror"];
                                                      });
var h$webkit_dom_document_get_webkit_visibility_state;
h$webkit_dom_document_get_webkit_visibility_state = (function(self,
                                                     self_2)
                                                     {
                                                       h$ret1 = 0;
                                                       return h$encodeUtf8(self["webkitVisibilityState"]);
                                                     });
var h$webkit_dom_document_get_webkit_hidden;
h$webkit_dom_document_get_webkit_hidden = (function(self,
                                           self_2)
                                           {
                                             return self["webkitHidden"];
                                           });
var h$webkit_dom_document_get_security_policy;
h$webkit_dom_document_get_security_policy = (function(self,
                                             self_2)
                                             {
                                               h$ret1 = 0;
                                               return self["SecurityPolicy"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Css
h$webkit_dom_css_value_get_type = (function()
                                   {
                                     return h$g_get_type(CSSValue);
                                   });
var h$webkit_dom_css_value_set_css_text;
h$webkit_dom_css_value_set_css_text = (function(self,
                                       self_2, val, val_2)
                                       {
                                         self["cssText"] = h$decodeUtf8z(val,
                                         val_2);
                                       });
var h$webkit_dom_css_value_get_css_text;
h$webkit_dom_css_value_get_css_text = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return h$encodeUtf8(self["cssText"]);
                                       });
var h$webkit_dom_css_value_get_css_value_type;
h$webkit_dom_css_value_get_css_value_type = (function(self,
                                             self_2)
                                             {
                                               return self["cssValueType"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Css
h$webkit_dom_css_style_sheet_get_type = (function()
                                         {
                                           return h$g_get_type(CSSStyleSheet);
                                         });
var h$webkit_dom_css_style_sheet_insert_rule;
h$webkit_dom_css_style_sheet_insert_rule = (function(self,
                                            self_2, rule, rule_2, index)
                                            {
                                              return self["insertRule"](h$decodeUtf8z(rule,
                                              rule_2), index);
                                            });
var h$webkit_dom_css_style_sheet_delete_rule;
h$webkit_dom_css_style_sheet_delete_rule = (function(self,
                                            self_2, index)
                                            {
                                              return self["deleteRule"](index);
                                            });
var h$webkit_dom_css_style_sheet_add_rule;
h$webkit_dom_css_style_sheet_add_rule = (function(self,
                                         self_2, selector, selector_2,
                                         style, style_2, index)
                                         {
                                           return self["addRule"](h$decodeUtf8z(selector,
                                           selector_2),
                                           h$decodeUtf8z(style, style_2),
                                           index);
                                         });
var h$webkit_dom_css_style_sheet_remove_rule;
h$webkit_dom_css_style_sheet_remove_rule = (function(self,
                                            self_2, index)
                                            {
                                              return self["removeRule"](index);
                                            });
var h$webkit_dom_css_style_sheet_get_owner_rule;
h$webkit_dom_css_style_sheet_get_owner_rule = (function(self,
                                               self_2)
                                               {
                                                 h$ret1 = 0;
                                                 return self["ownerRule"];
                                               });
var h$webkit_dom_css_style_sheet_get_css_rules;
h$webkit_dom_css_style_sheet_get_css_rules = (function(self,
                                              self_2)
                                              {
                                                h$ret1 = 0;
                                                return self["cssRules"];
                                              });
var h$webkit_dom_css_style_sheet_get_rules;
h$webkit_dom_css_style_sheet_get_rules = (function(self,
                                          self_2)
                                          {
                                            h$ret1 = 0;
                                            return self["rules"];
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Css
h$webkit_dom_css_style_declaration_get_type = (function()
                                               {
                                                 return h$g_get_type(CSSStyleDeclaration);
                                               });
var h$webkit_dom_css_style_declaration_get_property_value;
h$webkit_dom_css_style_declaration_get_property_value = (function(self,
                                                         self_2, propertyName,
                                                         propertyName_2)
                                                         {
                                                           h$ret1 = 0;
                                                           return h$encodeUtf8(self["getPropertyValue"](h$decodeUtf8z(propertyName,
                                                           propertyName_2)));
                                                         });
var h$webkit_dom_css_style_declaration_get_property_css_value;
h$webkit_dom_css_style_declaration_get_property_css_value = (function(self,
                                                             self_2,
                                                             propertyName,
                                                             propertyName_2)
                                                             {
                                                               h$ret1 = 0;
                                                               return self["getPropertyCSSValue"](h$decodeUtf8z(propertyName,
                                                               propertyName_2));
                                                             });
var h$webkit_dom_css_style_declaration_remove_property;
h$webkit_dom_css_style_declaration_remove_property = (function(self,
                                                      self_2, propertyName,
                                                      propertyName_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return h$encodeUtf8(self["removeProperty"](h$decodeUtf8z(propertyName,
                                                        propertyName_2)));
                                                      });
var h$webkit_dom_css_style_declaration_get_property_priority;
h$webkit_dom_css_style_declaration_get_property_priority = (function(self,
                                                            self_2,
                                                            propertyName,
                                                            propertyName_2)
                                                            {
                                                              h$ret1 = 0;
                                                              return h$encodeUtf8(self["getPropertyPriority"](h$decodeUtf8z(propertyName,
                                                              propertyName_2)));
                                                            });
var h$webkit_dom_css_style_declaration_set_property;
h$webkit_dom_css_style_declaration_set_property = (function(self,
                                                   self_2, propertyName,
                                                   propertyName_2, value,
                                                   value_2, priority,
                                                   priority_2)
                                                   {
                                                     return self["setProperty"](h$decodeUtf8z(propertyName,
                                                     propertyName_2),
                                                     h$decodeUtf8z(value,
                                                     value_2),
                                                     h$decodeUtf8z(priority,
                                                     priority_2));
                                                   });
var h$webkit_dom_css_style_declaration_item;
h$webkit_dom_css_style_declaration_item = (function(self,
                                           self_2, index)
                                           {
                                             h$ret1 = 0;
                                             return h$encodeUtf8(self["item"](index));
                                           });
var h$webkit_dom_css_style_declaration_get_property_shorthand;
h$webkit_dom_css_style_declaration_get_property_shorthand = (function(self,
                                                             self_2,
                                                             propertyName,
                                                             propertyName_2)
                                                             {
                                                               h$ret1 = 0;
                                                               return h$encodeUtf8(self["getPropertyShorthand"](h$decodeUtf8z(propertyName,
                                                               propertyName_2)));
                                                             });
var h$webkit_dom_css_style_declaration_is_property_implicit;
h$webkit_dom_css_style_declaration_is_property_implicit = (function(self,
                                                           self_2, propertyName,
                                                           propertyName_2)
                                                           {
                                                             return self["isPropertyImplicit"](h$decodeUtf8z(propertyName,
                                                             propertyName_2));
                                                           });
var h$webkit_dom_css_style_declaration_set_css_text;
h$webkit_dom_css_style_declaration_set_css_text = (function(self,
                                                   self_2, val, val_2)
                                                   {
                                                     self["cssText"] = h$decodeUtf8z(val,
                                                     val_2);
                                                   });
var h$webkit_dom_css_style_declaration_get_css_text;
h$webkit_dom_css_style_declaration_get_css_text = (function(self,
                                                   self_2)
                                                   {
                                                     h$ret1 = 0;
                                                     return h$encodeUtf8(self["cssText"]);
                                                   });
var h$webkit_dom_css_style_declaration_get_length;
h$webkit_dom_css_style_declaration_get_length = (function(self,
                                                 self_2)
                                                 {
                                                   return self["length"];
                                                 });
var h$webkit_dom_css_style_declaration_get_parent_rule;
h$webkit_dom_css_style_declaration_get_parent_rule = (function(self,
                                                      self_2)
                                                      {
                                                        h$ret1 = 0;
                                                        return self["parentRule"];
                                                      });
// Graphics.UI.Gtk.WebKit.DOM.Css
h$webkit_dom_css_rule_list_get_type = (function()
                                       {
                                         return h$g_get_type(CSSRuleList);
                                       });
var h$webkit_dom_css_rule_list_item;
h$webkit_dom_css_rule_list_item = (function(self,
                                   self_2, index)
                                   {
                                     h$ret1 = 0;
                                     return self["item"](index);
                                   });
var h$webkit_dom_css_rule_list_get_length;
h$webkit_dom_css_rule_list_get_length = (function(self,
                                         self_2)
                                         {
                                           return self["length"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Css
h$webkit_dom_css_rule_get_type = (function()
                                  {
                                    return h$g_get_type(CSSRule);
                                  });
var h$webkit_dom_css_rule_set_css_text;
h$webkit_dom_css_rule_set_css_text = (function(self,
                                      self_2, val, val_2)
                                      {
                                        self["cssText"] = h$decodeUtf8z(val,
                                        val_2);
                                      });
var h$webkit_dom_css_rule_get_css_text;
h$webkit_dom_css_rule_get_css_text = (function(self,
                                      self_2)
                                      {
                                        h$ret1 = 0;
                                        return h$encodeUtf8(self["cssText"]);
                                      });
var h$webkit_dom_css_rule_get_parent_style_sheet;
h$webkit_dom_css_rule_get_parent_style_sheet = (function(self,
                                                self_2)
                                                {
                                                  h$ret1 = 0;
                                                  return self["parentStyleSheet"];
                                                });
var h$webkit_dom_css_rule_get_parent_rule;
h$webkit_dom_css_rule_get_parent_rule = (function(self,
                                         self_2)
                                         {
                                           h$ret1 = 0;
                                           return self["parentRule"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_console_get_type = (function()
                                 {
                                   return h$g_get_type(Console);
                                 });
var h$webkit_dom_console_time;
h$webkit_dom_console_time = (function(self,
                             self_2, title, title_2)
                             {
                               return self["time"](h$decodeUtf8z(title,
                               title_2));
                             });
var h$webkit_dom_console_group_end;
h$webkit_dom_console_group_end = (function(self,
                                  self_2)
                                  {
                                    return self["groupEnd"]();
                                  });
var h$webkit_dom_console_get_memory;
h$webkit_dom_console_get_memory = (function(self,
                                   self_2)
                                   {
                                     h$ret1 = 0;
                                     return self["memory"];
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_comment_get_type = (function()
                                 {
                                   return h$g_get_type(Comment);
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_character_data_get_type = (function()
                                        {
                                          return h$g_get_type(CharacterData);
                                        });
var h$webkit_dom_character_data_substring_data;
h$webkit_dom_character_data_substring_data = (function(self,
                                              self_2, offset, length)
                                              {
                                                h$ret1 = 0;
                                                return h$encodeUtf8(self["substringData"](offset,
                                                length));
                                              });
var h$webkit_dom_character_data_append_data;
h$webkit_dom_character_data_append_data = (function(self,
                                           self_2, data, data_2)
                                           {
                                             return self["appendData"](h$decodeUtf8z(data,
                                             data_2));
                                           });
var h$webkit_dom_character_data_insert_data;
h$webkit_dom_character_data_insert_data = (function(self,
                                           self_2, offset, data, data_2)
                                           {
                                             return self["insertData"](offset,
                                             h$decodeUtf8z(data, data_2));
                                           });
var h$webkit_dom_character_data_delete_data;
h$webkit_dom_character_data_delete_data = (function(self,
                                           self_2, offset, length)
                                           {
                                             return self["deleteData"](offset,
                                             length);
                                           });
var h$webkit_dom_character_data_replace_data;
h$webkit_dom_character_data_replace_data = (function(self,
                                            self_2, offset, length, data,
                                            data_2)
                                            {
                                              return self["replaceData"](offset,
                                              length, h$decodeUtf8z(data,
                                              data_2));
                                            });
var h$webkit_dom_character_data_set_data;
h$webkit_dom_character_data_set_data = (function(self,
                                        self_2, val, val_2)
                                        {
                                          self["data"] = h$decodeUtf8z(val,
                                          val_2);
                                        });
var h$webkit_dom_character_data_get_data;
h$webkit_dom_character_data_get_data = (function(self,
                                        self_2)
                                        {
                                          h$ret1 = 0;
                                          return h$encodeUtf8(self["data"]);
                                        });
var h$webkit_dom_character_data_get_length;
h$webkit_dom_character_data_get_length = (function(self,
                                          self_2)
                                          {
                                            return self["length"];
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_cdata_section_get_type = (function()
                                       {
                                         return h$g_get_type(CDATASection);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
h$webkit_dom_blob_get_type = (function()
                              {
                                return h$g_get_type(Blob);
                              });
var h$webkit_dom_blob_get_size;
h$webkit_dom_blob_get_size = (function(self,
                              self_2)
                              {
                                return self["size"];
                              });
// Graphics.UI.Gtk.WebKit.DOM.Window
h$webkit_dom_bar_info_get_type = (function()
                                  {
                                    return h$g_get_type(BarInfo);
                                  });
var h$webkit_dom_bar_info_get_visible;
h$webkit_dom_bar_info_get_visible = (function(self,
                                     self_2)
                                     {
                                       return self["visible"];
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Core
h$webkit_dom_attr_get_type = (function()
                              {
                                return h$g_get_type(Attr);
                              });
var h$webkit_dom_attr_get_name;
h$webkit_dom_attr_get_name = (function(self,
                              self_2)
                              {
                                h$ret1 = 0;
                                return h$encodeUtf8(self["name"]);
                              });
var h$webkit_dom_attr_get_specified;
h$webkit_dom_attr_get_specified = (function(self,
                                   self_2)
                                   {
                                     return self["specified"];
                                   });
var h$webkit_dom_attr_set_value;
h$webkit_dom_attr_set_value = (function(self,
                               self_2, val, val_2)
                               {
                                 self["value"] = h$decodeUtf8z(val,
                                 val_2);
                               });
var h$webkit_dom_attr_get_value;
h$webkit_dom_attr_get_value = (function(self,
                               self_2)
                               {
                                 h$ret1 = 0;
                                 return h$encodeUtf8(self["value"]);
                               });
var h$webkit_dom_attr_get_owner_element;
h$webkit_dom_attr_get_owner_element = (function(self,
                                       self_2)
                                       {
                                         h$ret1 = 0;
                                         return self["ownerElement"];
                                       });
var h$webkit_dom_attr_get_is_id;
h$webkit_dom_attr_get_is_id = (function(self,
                               self_2)
                               {
                                 return self["isId"];
                               });
