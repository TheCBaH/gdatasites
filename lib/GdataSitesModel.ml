let namespace = "http://schemas.google.com/sites/2008"

let endpoint = "https://sites.google.com/feeds/"

let ns = "site"

let ns_sites = namespace

let get_sites_prefix namespace' =
  if namespace' = namespace then "docs" else GdataACL.get_acl_prefix ns


let parse_xml_data_model tree =
  match tree with
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


module Content = struct
  module Entry = struct
    type content =
      | Empty
      | Attachment of string * string
      | Xhtml of GdataCore.xml_data_model

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
      ; extensions: GdataCore.xml_data_model list
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
      ; extensions= []
      ; content= Empty }


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
        ; entry.extensions ]


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
          {entry with content= Xhtml cs}
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
          let extensions = tree :: entry.extensions in
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


  let make_category name =
    let category_scheme = "http://schemas.google.com/g/2005#kind" in
    { GdataAtom.Category.empty with
      scheme= category_scheme
    ; term= "http://schemas.google.com/sites/2008#" ^ name
    ; label= name }


  let attachment_category = make_category "attachment"

  let webpage_category = make_category "webpage"
end

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
      ; extensions: GdataCore.xml_data_model list }

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
      ; extensions= [] }


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
        ; entry.extensions ]


    let of_xml_data_model entry tree =
      match parse_xml_data_model tree with
      | `Etag etag -> {entry with etag}
      | `Id id -> {entry with id}
      | `Updated updated -> {entry with updated}
      | `Edited edited -> {entry with edited}
      | `Summary summary -> {entry with summary}
      | `Title title -> {entry with title}
      | `Link link -> {entry with links= link :: entry.links}
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
          let extensions = tree :: entry.extensions in
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
