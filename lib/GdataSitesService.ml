open GdataSitesModel

let endpoint = "https://sites.google.com/feeds/"

let version = "1.4"

let parse_content_feed = GdataUtils.parse_xml_response Content.parse_feed

let parse_content_entry = GdataUtils.parse_xml_response Content.parse_entry

let parse_site_feed = GdataUtils.parse_xml_response Site.parse_feed

let parse_site_entry = GdataUtils.parse_xml_response Site.parse_entry

let get_url ?(rel= `Edit) links = GdataAtom.find_url rel links

let get_media_url ?(rel= `EditMedia) links = GdataAtom.find_url rel links

let get_etag etag = GdataUtils.string_to_option etag

let get_url_etag_content ?rel entry =
  let url = get_url entry.Content.Entry.links ?rel in
  let etag = get_etag entry.Content.Entry.etag in
  (url, etag)


let get_content base_url resource_id session =
  let url = GapiUtils.add_path_to_url ~encoded:false [resource_id] base_url in
  GdataService.query ~version url parse_content_entry session


type basic_parameters =
  [ `Max_results of int
  | `Published_max of Netdate.t
  | `Published_min of Netdate.t
  | `Q of string
  | `Start_index of int ]

let query_to_key_value = function
  | `Max_results n -> ("max-results", string_of_int n)
  | `Published_max date -> ("published-max", GapiDate.to_string date)
  | `Published_min date -> ("published-min", GapiDate.to_string date)
  | `Start_index n -> ("published-min", string_of_int n)
  | `Q s -> ("q", s)


let parameters_of_query f query = Option.map (List.map f) query

let query_site ?(domain= "site") ?site ?etag ?query session =
  let url = endpoint ^ "site/" ^ domain ^ "/" in
  let url = match site with Some site -> url ^ site ^ "/" | None -> url in
  let query_parameters =
    parameters_of_query
      (function
          | `Include_all_sites b -> ("include-all-sites", string_of_bool b)
          | `With_mappings b -> ("with-mappings", string_of_bool b)
          | #basic_parameters as q -> query_to_key_value q)
      query
  in
  GdataService.query ~version ?etag ?query_parameters url parse_site_feed
    session


let string_of_category = function
  | `Announcement -> "announcement"
  | `AnnouncementPage -> "announcementpage"
  | `Attachment -> "attachment"
  | `Comment -> "comment"
  | `FileCabinet -> "filecabinet"
  | `ListItem -> "listitem"
  | `ListPage -> "listpage"
  | `WebPage -> "webpage"
  | `WebAttachment -> "webattachment"
  | `Template -> "template"


let content_url ?(domain= "site") site =
  endpoint ^ "content/" ^ domain ^ "/" ^ site ^ "/"


let query_content_list ?domain ?etag ?query site session =
  let url = content_url ?domain site in
  let query_parameters =
    parameters_of_query
      (function
          | `Updated_max date -> ("published-max", GapiDate.to_string date)
          | `Updated_min date -> ("published-min", GapiDate.to_string date)
          | `Ancestor s -> ("ancestor", s)
          | `Include_deleted b -> ("include-deleted", string_of_bool b)
          | `Include_draft b -> ("include-draft", string_of_bool b)
          | `Category c -> ("category", string_of_category c)
          | `Parent s -> ("parent", s)
          | `Path p -> ("path", "/" ^ String.concat "/" p)
          | #basic_parameters as q -> query_to_key_value q)
      query
  in
  GdataService.query ~version ?query_parameters ?etag url parse_content_feed
    session


let media_source_payload media =
  match media.GapiMediaResource.source with
  | GapiMediaResource.String s -> s
  | _ -> failwith "media_source_payload not supported"


let make_payload ?media_source data_to_tree data =
  match media_source with
  | None ->
      let tree = Content.entry_to_data_model data in
      let xml = GdataUtils.data_to_xml_string tree in
      GapiCore.PostData.Body
        (GapiCore.PostData.String xml, GdataCore.default_content_type)
  | Some media_source ->
      let payload = media_source_payload media_source in
      GapiCore.PostData.Body
        ( GapiCore.PostData.String payload
        , media_source.GapiMediaResource.content_type )


let make_multipart ?media_source data_to_tree data =
  let tree = Content.entry_to_data_model data in
  let xml = GdataUtils.data_to_xml_string tree in
  match media_source with
  | None ->
      GapiCore.PostData.Body
        (GapiCore.PostData.String xml, GdataCore.default_content_type)
  | Some media_source ->
      let nl = "\r\n" in
      let make_content_type content_type = "Content-Type: " ^ content_type in
      let boundary = "END_OF_PART" in
      let content_type = "multipart/related; boundary=" ^ boundary in
      let boundary = "--" ^ boundary ^ nl in
      let body =
        String.concat ""
          [ "Media multipart posting"
          ; nl
          ; boundary
          ; make_content_type GdataCore.default_content_type
          ; nl
          ; nl
          ; xml
          ; nl
          ; boundary
          ; make_content_type media_source.GapiMediaResource.content_type
          ; nl
          ; nl
          ; media_source_payload media_source
          ; nl
          ; boundary ]
      in
      GapiCore.PostData.Body (GapiCore.PostData.String body, content_type)


let create_content ?domain ?media_source site entry session =
  let url = content_url ?domain site in
  GapiService.service_request_with_data GapiRequest.Create
    (make_multipart ?media_source Content.entry_to_data_model)
    ~version entry url
    (fun pipe _ -> parse_content_entry pipe)
    session


let create_attachment ?domain ?summary ~parent ~title ~content_type ~payload
    ~site session =
  let media_source =
    { GapiMediaResource.source= GapiMediaResource.String payload
    ; name= title
    ; content_type
    ; content_length= String.length payload |> Int64.of_int }
  in
  let entry =
    { Content.Entry.empty with
      Content.Entry.category= Content.attachment_category
    ; title= {GdataAtom.Title.empty with GdataAtom.Title.value= title}
    ; links= [Content.make_link `Parent parent] }
  in
  let entry =
    match summary with
    | None -> entry
    | Some summary ->
        { entry with
          Content.Entry.summary=
            {entry.Content.Entry.summary with GdataAtom.Summary.value= summary}
        }
  in
  create_content ?domain ~media_source site entry session


let create_webpage ?domain ?summary ?parent ~title ?pageName ?html ?template
    ~site session =
  let links = [] in
  let links =
    match parent with
    | Some parent -> Content.make_link `Parent parent :: links
    | None -> links
  in
  let links =
    match template with
    | Some template -> Content.make_link `Template template :: links
    | None -> links
  in
  let entry =
    { Content.Entry.empty with
      Content.Entry.category= Content.webpage_category
    ; title= {GdataAtom.Title.empty with GdataAtom.Title.value= title}
    ; links }
  in
  let entry =
    match pageName with
    | None -> entry
    | Some pageName -> {entry with Content.Entry.pageName}
  in
  let entry =
    match html with
    | None -> entry
    | Some html -> {entry with Content.Entry.content= Content.Entry.Xhtml html}
  in
  let entry =
    match summary with
    | None -> entry
    | Some summary ->
        { entry with
          Content.Entry.summary=
            {entry.Content.Entry.summary with GdataAtom.Summary.value= summary}
        }
  in
  create_content ?domain site entry session


let delete_content entry session =
  let url, etag = get_url_etag_content entry in
  GdataService.delete ~version ?etag url session


let update_content ?media_source entry session =
  let url, etag = get_url_etag_content entry in
  let url =
    match media_source with
    | Some _ -> get_media_url entry.Content.Entry.links
    | None -> url
  in
  GapiService.service_request_with_data GapiRequest.Update
    (make_multipart ?media_source Content.entry_to_data_model)
    ~version ?etag entry url
    (fun pipe _ -> parse_content_entry pipe)
    session

