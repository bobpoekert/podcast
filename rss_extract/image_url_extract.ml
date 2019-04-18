open Lib_rss_extract

let rec get_tag_path tags xml = 
  match tags with 
  | [] -> []
  | tag :: rst -> 
    match xml with 
    | Xml.PCData(_) -> []
    | Xml.Element(xtag, _attrs, children) -> (
      if (String.equal tag xtag) then(
        match rst with
        | [] -> [xml] 
        | _ -> List.fold_left (fun res x -> List.append res (get_tag_path rst x)) [] children)
      else
        List.fold_left (fun res x -> List.append res (get_tag_path tags x)) [] children
    )

let rec _get_text res xml =
  match xml with 
  | Xml.PCData(v) -> (v :: res)
  | Xml.Element(_tag, _attrs, children) -> 
    List.fold_left (fun res x -> List.append res (_get_text res x)) [] children

let get_text xmls = _get_text [] xmls

let get_image_urls xml =
  xml 
  |> get_tag_path ["rss"; "channel"; "image"; "url"]
  |> List.map get_text
  |> List.concat

let () = 
  let argc = (Array.length Sys.argv) - 1 in
  for idx = 1 to argc do 
    iter_xml_pages (Array.get Sys.argv idx) (fun _req headers _head xml ->
      let url = Warc.get_url headers in 
      let url_hash = url |> Sha1.string |> Sha1.to_hex in 
      let image_urls = get_image_urls xml in
      match image_urls with 
      | [] -> ()
      | image_urls -> 
        Printf.printf "%s\n\tout=%s\n" (String.concat "\t" image_urls) url_hash;
    )
  done