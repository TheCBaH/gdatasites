open GdataSitesModel

let endpoint = "https://sites.google.com/feeds/"

let version = "1.4"

let parse_content_feed = GdataUtils.parse_xml_response Content.parse_feed

let parse_content_entry = GdataUtils.parse_xml_response Content.parse_entry

let parse_site_feed = GdataUtils.parse_xml_response Site.parse_feed

let parse_site_entry = GdataUtils.parse_xml_response Site.parse_entry

let get_url ?(rel = `Edit) links =
  GdataAtom.find_url rel links

let get_media_url ?(rel = `EditMedia) links =
  GdataAtom.find_url rel links

let get_etag etag =
  GdataUtils.string_to_option etag

let get_url_etag_content ?rel entry =
  let url = get_url entry.Content.Entry.links ?rel in
  let etag = get_etag entry.Content.Entry.etag in
    (url, etag)


let get_content base_url resource_id session =
  let url = GapiUtils.add_path_to_url ~encoded:false [resource_id] base_url in
  GdataService.query ~version url parse_content_entry session


let query_content_list ~url ?etag ?parameters session =
  GdataService.query ~version ?etag url parse_content_feed session


let query_site ~url ?etag ?parameters session =
  GdataService.query ~version ?etag url parse_site_feed session


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
        (GapiCore.PostData.String payload, media_source.GapiMediaResource.content_type)


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


let create_content ~url ?media_source entry session =
  GapiService.service_request_with_data GapiRequest.Create
    (make_multipart ?media_source Content.entry_to_data_model)
    ~version entry url
    (fun pipe _ -> parse_content_entry pipe)
    session

let delete_content entry session =
  let (url,etag) = get_url_etag_content entry in
  GdataService.delete ~version ?etag url session

let update_content ?media_source entry session =
  let (url,etag) = get_url_etag_content entry in
  let (make,url) = match media_source with
      Some _ -> make_payload,get_media_url entry.Content.Entry.links
    | None -> make_multipart,url in
  "Update with " ^ url |> print_endline;
  GapiService.service_request_with_data GapiRequest.Update
    (make ?media_source Content.entry_to_data_model)
    ~version ?etag entry url
    (fun pipe _ -> parse_content_entry pipe)
    session
