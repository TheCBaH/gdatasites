let ns_sites = "http://schemas.google.com/sites/2008"

let endpoint = "https://sites.google.com/feeds/"

let get_sites_prefix namespace =
  if namespace = ns_sites then "site" else GdataACL.get_acl_prefix namespace


let parse_xml_data_model tree =
  match tree with
  | GapiCore.AnnotatedTree.Leaf
      ( [`Attribute; (`Name ns); (`Namespace "http://www.w3.org/2000/xmlns/")]
      , _ ) ->
      `Namespace
  | GapiCore.AnnotatedTree.Leaf
      ([`Attribute; (`Name "etag"); (`Namespace ns)], v)
    when ns = GdataAtom.ns_gd ->
      `Etag v
  | GapiCore.AnnotatedTree.Node
      ( [`Element; (`Name "id"); (`Namespace ns)]
      , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
    when ns = GdataAtom.ns_atom ->
      `Id v
  | GapiCore.AnnotatedTree.Node
      ( [`Element; (`Name "updated"); (`Namespace ns)]
      , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
    when ns = GdataAtom.ns_atom ->
      `Updated (GapiDate.of_string v)
  | GapiCore.AnnotatedTree.Node
      ( [`Element; (`Name "published"); (`Namespace ns)]
      , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
    when ns = GdataAtom.ns_atom ->
      `Published (GapiDate.of_string v)
  | GapiCore.AnnotatedTree.Node
      ( [`Element; (`Name "edited"); (`Namespace ns)]
      , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
    when ns = GdataAtom.ns_app ->
      `Edited (GapiDate.of_string v)
  | GapiCore.AnnotatedTree.Node
      ( [`Element; (`Name "edited"); (`Namespace ns)]
      , [_; (GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
    when ns = GdataAtom.ns_app ->
      `Edited (GapiDate.of_string v)
  | GapiCore.AnnotatedTree.Node
      ([`Element; (`Name "summary"); (`Namespace ns)], cs)
    when ns = GdataAtom.ns_atom ->
      GdataAtom.parse_children GdataAtom.Summary.of_xml_data_model
        GdataAtom.Summary.empty
        (fun summary -> `Summary summary)
        cs
  | GapiCore.AnnotatedTree.Node
      ([`Element; (`Name "title"); (`Namespace ns)], cs)
    when ns = GdataAtom.ns_atom ->
      GdataAtom.parse_children GdataAtom.Title.of_xml_data_model
        GdataAtom.Title.empty
        (fun title -> `Title title)
        cs
  | GapiCore.AnnotatedTree.Node
      ([`Element; (`Name "link"); (`Namespace ns)], cs)
    when ns = GdataAtom.ns_atom ->
      GdataAtom.parse_children GdataAtom.Link.of_xml_data_model
        GdataAtom.Link.empty
        (fun link -> `Link link)
        cs
  | GapiCore.AnnotatedTree.Node
      ([`Element; (`Name "author"); (`Namespace ns)], cs)
    when ns = GdataAtom.ns_atom ->
      GdataAtom.parse_children GdataAtom.Author.of_xml_data_model
        GdataAtom.Author.empty
        (fun author -> `Author author)
        cs
  | GapiCore.AnnotatedTree.Node
      ([`Element; (`Name "category"); (`Namespace ns)], cs)
    when ns = GdataAtom.ns_atom ->
      GdataAtom.parse_children GdataAtom.Category.of_xml_data_model
        GdataAtom.Category.empty
        (fun category -> `Category category)
        cs
  | e -> `Other e


module Site = struct
  module Entry = struct
    type t =
      { etag: string
      ; id: GdataAtom.atom_id
      ; updated: GdataAtom.atom_updated
      ; title: GdataAtom.Title.t
      ; summary: GdataAtom.Summary.t
      ; edited: GdataAtom.app_edited
      ; links: GdataAtom.Link.t list
      ; siteName: string
      ; theme: string
      ; extensions: GdataAtom.GenericExtensions.t }

    let empty =
      { etag= ""
      ; id= ""
      ; updated= GapiDate.epoch
      ; edited= GapiDate.epoch
      ; summary= GdataAtom.Summary.empty
      ; title= GdataAtom.Title.empty
      ; links= []
      ; siteName= ""
      ; theme= ""
      ; extensions= GdataAtom.GenericExtensions.empty }


    let to_xml_data_model entry =
      GdataAtom.render_element GdataAtom.ns_atom "entry"
        [ GdataAtom.render_attribute GdataAtom.ns_gd "etag" entry.etag
        ; GdataAtom.render_text_element GdataAtom.ns_atom "id" entry.id
        ; GdataAtom.render_date_element GdataAtom.ns_atom "updated"
            entry.updated
        ; GdataAtom.render_date_element GdataAtom.ns_app "edited" entry.edited
        ; GdataAtom.render_element_list GdataAtom.Link.to_xml_data_model
            entry.links
        ; GdataAtom.Summary.to_xml_data_model entry.summary
        ; GdataAtom.Title.to_xml_data_model entry.title
        ; GdataAtom.render_text_element ns_sites "siteName" entry.siteName
        ; GdataAtom.render_text_element ns_sites "theme" entry.theme
        ; GdataAtom.GenericExtensions.to_xml_data_model entry.extensions ]


    let of_xml_data_model entry tree =
      match parse_xml_data_model tree with
      | `Etag etag -> {entry with etag}
      | `Id id -> {entry with id}
      | `Updated updated -> {entry with updated}
      | `Edited edited -> {entry with edited}
      | `Summary summary -> {entry with summary}
      | `Title title -> {entry with title}
      | `Link link -> {entry with links= link :: entry.links}
      | `Namespace -> entry
      | `Other
          GapiCore.AnnotatedTree.Node
            ( [`Element; (`Name "siteName"); (`Namespace ns)]
            , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
        when ns = ns_sites ->
          {entry with siteName= v}
      | `Other
          GapiCore.AnnotatedTree.Node
            ( [`Element; (`Name "theme"); (`Namespace ns)]
            , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
        when ns = ns_sites ->
          {entry with theme= v}
      | _ ->
          let extensions =
            GdataAtom.GenericExtensions.of_xml_data_model entry.extensions tree
          in
          {entry with extensions}

  end

  module Feed =
    GdataAtom.MakeFeed (Entry) (GdataAtom.Link) (GdataAtom.GenericExtensions)

  let parse_entry =
    GdataAtom.data_model_to_entry Entry.of_xml_data_model Entry.empty


  let parse_feed = Feed.parse_feed

  let entry_to_data_model =
    GdataAtom.element_to_data_model get_sites_prefix Entry.to_xml_data_model


  let feed_to_data_model =
    GdataAtom.element_to_data_model get_sites_prefix Feed.to_xml_data_model

end

module Content = struct
  module Category = struct
    let to_string = function
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


    let make category =
      let name = to_string category in
      let category_scheme = "http://schemas.google.com/g/2005#kind" in
      { GdataAtom.Category.empty with
        GdataAtom.Category.scheme= category_scheme
      ; term= "http://schemas.google.com/sites/2008#" ^ name
      ; label= name }


    let attachment = make `Attachment

    let webpage = make `WebPage
  end

  module Entry = struct
    type content = Empty | Attachment of string * string | Xhtml of string

    type t =
      { etag: string
      ; id: GdataAtom.atom_id
      ; updated: GdataAtom.atom_updated
      ; published: GdataAtom.atom_published
      ; edited: GdataAtom.app_edited
      ; category: GdataAtom.Category.t
      ; title: GdataAtom.Title.t
      ; links: GdataAtom.Link.t list
      ; author: GdataAtom.Author.t
      ; summary: GdataAtom.Summary.t
      ; pageName: string
      ; revision: int
      ; extensions: GdataAtom.GenericExtensions.t
      ; content: content }

    let empty =
      { etag= ""
      ; id= ""
      ; updated= GapiDate.epoch
      ; published= GapiDate.epoch
      ; edited= GapiDate.epoch
      ; category= GdataAtom.Category.empty
      ; title= GdataAtom.Title.empty
      ; summary= GdataAtom.Summary.empty
      ; links= []
      ; author= GdataAtom.Author.empty
      ; pageName= ""
      ; revision= 0
      ; extensions= GdataAtom.GenericExtensions.empty
      ; content= Empty }


    let content_to_xml_data_model = function
      | Empty | Attachment _ -> []
      | Xhtml html ->
          let string_input str =
            let off = ref 0 in
            fun () ->
              if !off < String.length str then
                let ch = str.[!off] in
                incr off ; Char.code ch
              else raise End_of_file
          in
          let xhtml = GdataUtils.parse_xml (string_input html) (fun x -> x) in
          [ GapiCore.AnnotatedTree.Node
              ( [`Element; `Name "content"; `Namespace GdataAtom.ns_atom]
              , [ GapiCore.AnnotatedTree.Leaf
                    ([`Attribute; `Name "type"; `Namespace ""], "xhtml")
                ; xhtml ] ) ]


    let to_xml_data_model entry =
      GdataAtom.render_element GdataAtom.ns_atom "entry"
        [ GdataAtom.render_attribute GdataAtom.ns_gd "etag" entry.etag
        ; GdataAtom.render_text_element GdataAtom.ns_atom "id" entry.id
        ; GdataAtom.render_date_element GdataAtom.ns_atom "updated"
            entry.updated
        ; GdataAtom.render_date_element GdataAtom.ns_app "published"
            entry.published
        ; GdataAtom.render_date_element GdataAtom.ns_app "edited" entry.edited
        ; GdataAtom.Category.to_xml_data_model entry.category
        ; GdataAtom.Title.to_xml_data_model entry.title
        ; GdataAtom.Summary.to_xml_data_model entry.summary
        ; GdataAtom.render_element_list GdataAtom.Link.to_xml_data_model
            entry.links
        ; GdataAtom.Author.to_xml_data_model entry.author
        ; GdataAtom.render_text_element ns_sites "pageName" entry.pageName
        ; GdataAtom.render_int_element ns_sites "revision" entry.revision
        ; content_to_xml_data_model entry.content
        ; GdataAtom.GenericExtensions.to_xml_data_model entry.extensions ]


    let of_xml_data_model entry tree =
      match parse_xml_data_model tree with
      | `Etag etag -> {entry with etag}
      | `Id id -> {entry with id}
      | `Updated updated -> {entry with updated}
      | `Edited edited -> {entry with edited}
      | `Category category -> {entry with category}
      | `Published published -> {entry with published}
      | `Title title -> {entry with title}
      | `Link link -> {entry with links= link :: entry.links}
      | `Author author -> {entry with author}
      | `Summary summary -> {entry with summary}
      | `Namespace -> entry
      | `Other
          GapiCore.AnnotatedTree.Node
            ( [`Element; (`Name "pageName"); (`Namespace ns)]
            , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
        when ns = ns_sites ->
          {entry with pageName= v}
      | `Other
          GapiCore.AnnotatedTree.Node
            ( [`Element; (`Name "revision"); (`Namespace ns)]
            , [(GapiCore.AnnotatedTree.Leaf ([`Text], v))] )
        when ns = ns_sites ->
          {entry with revision= int_of_string v}
      | `Other
          GapiCore.AnnotatedTree.Node
            ( [`Element; (`Name "content"); (`Namespace ns)]
            , [ (GapiCore.AnnotatedTree.Leaf
                  ([`Attribute; (`Name "type"); (`Namespace "")], "xhtml"))
              ; (GapiCore.AnnotatedTree.Node _ as cs) ] )
        when ns = GdataAtom.ns_atom ->
          let html = GdataUtils.data_to_xml_string cs in
          {entry with content= Xhtml html}
      | `Other
          GapiCore.AnnotatedTree.Node
            ( [`Element; (`Name "content"); (`Namespace ns)]
            , [ (GapiCore.AnnotatedTree.Leaf
                  ([`Attribute; (`Name "type"); (`Namespace "")], content_type))
              ; (GapiCore.AnnotatedTree.Leaf
                  ([`Attribute; (`Name "src"); (`Namespace "")], src)) ] )
        when ns = GdataAtom.ns_atom ->
          {entry with content= Attachment (content_type, src)}
      | _ ->
          let extensions =
            GdataAtom.GenericExtensions.of_xml_data_model entry.extensions tree
          in
          {entry with extensions}

  end

  module Feed =
    GdataAtom.MakeFeed (Entry) (GdataAtom.Link) (GdataAtom.GenericExtensions)

  let parse_entry =
    GdataAtom.data_model_to_entry Entry.of_xml_data_model Entry.empty


  let parse_feed = Feed.parse_feed

  let entry_to_data_model =
    GdataAtom.element_to_data_model get_sites_prefix Entry.to_xml_data_model


  let feed_to_data_model =
    GdataAtom.element_to_data_model get_sites_prefix Feed.to_xml_data_model


  module Rel = struct
    type t = [`Self | `Alternate | `Edit | `Media | `Parent | `Template]

    let parent = "http://schemas.google.com/sites/2008#parent"

    let template = "http://schemas.google.com/sites/2008#template"

    let to_string (t: t) =
      match t with
      | `Template -> template
      | `Parent -> parent
      | `Self as r -> GdataAtom.Rel.to_string r
      | `Alternate as r -> GdataAtom.Rel.to_string r
      | `Edit as r -> GdataAtom.Rel.to_string r
      | `Media as r -> GdataAtom.Rel.to_string r

  end

  let get_link rel c = GdataAtom.find_url_generic (module Rel) rel

  let make_link rel entry =
    let link =
      { GdataAtom.Link.empty with
        GdataAtom.Link.rel= Rel.to_string rel; href= entry.Entry.id }
    in
    match rel with
    | `Template | `Parent ->
        {link with GdataAtom.Link._type= "application/atom+xml"}
    | _ -> link

end
