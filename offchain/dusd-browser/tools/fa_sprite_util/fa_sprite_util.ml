(*
A script that fixes up some of our requirements for the SVG sprites
provided by Font Awesome.

• given a list of arguments for icon names to hold onto, delete the rest
• insert metadata/licencing in accordance with common namespaces rather than a comment
*)

let keep_only_icons icon_names soup =
	(* :not() with multiple not supported :(
	let as_ids = List.map (fun n -> "#" ^ n) icon_names in
	let selector = String.concat "" [ "symbol:not(" ; (String.concat ", " as_ids); ")" ] in
	Soup.select selector soup |> Soup.iter Soup.delete
	*)
	let id_allowed node =
		match Soup.id node with
		| Some id -> List.mem id icon_names
		| None -> false
	in
	Soup.select "symbol" soup
	|> Soup.iter (fun n -> if not (id_allowed n) then Soup.delete n)

let attach_metadata fa_version fa_year soup =
	match Soup.select_one "svg" soup with
	| Some svg ->
		let () = Soup.set_attribute "xmlns:rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" svg in
		let () = Soup.set_attribute "xmlns:cc" "http://creativecommons.org/ns#" svg in
		let () = Soup.set_attribute "xmlns:dc" "http://purl.org/dc/elements/1.1/" svg in
		(* Font Awesome _requires_ attribution to comply with CC BY *)
		let namespaced_metadata = {|
			<rdf:RDF>
				<cc:Work rdf:about="">
					<cc:license rdf:resource="http://creativecommons.org/licenses/by/4.0/"/>
					<dc:title>Font Awesome Free |} ^ fa_version ^ {|</dc:title>
					<dc:date>|} ^ fa_year ^ {|</dc:date>
					<dc:creator>
						<cc:Agent>
							<dc:title>@fontawesome</dc:title>
						</cc:Agent>
					</dc:creator>
				</cc:Work>
				<cc:License rdf:about="http://creativecommons.org/licenses/by/4.0/">
					<cc:permits rdf:resource="http://creativecommons.org/ns#Reproduction"/>
					<cc:permits rdf:resource="http://creativecommons.org/ns#Distribution"/>
					<cc:requires rdf:resource="http://creativecommons.org/ns#Notice"/>
					<cc:requires rdf:resource="http://creativecommons.org/ns#Attribution"/>
					<cc:permits rdf:resource="http://creativecommons.org/ns#DerivativeWorks"/>
				</cc:License>
			</rdf:RDF>
		|} in
		let metadata = Soup.create_element "metadata" in
		let () = Soup.append_child metadata (Soup.parse namespaced_metadata) in
		Soup.prepend_child svg metadata
	| None -> ()

let () =
	let fa_version = ref ""
	and fa_year = ref ""
	and icon_names = ref []
	in

	let cmd_opts =
		[ ( 'V', "font-awesome-version", None, (Getopt.atmost_once fa_version (Getopt.Error "Only one Font Awesome version allowed")) )
		; ( 'Y', "font-awesome-year", None, (Getopt.atmost_once fa_year (Getopt.Error "Only one Font Awesome year allowed")) )
		; ( 'i', "icon", None, (Getopt.append icon_names) )
		]
	in
	let _ = Getopt.parse_cmdline cmd_opts print_endline in

	let soup = Markup.channel stdin |> Markup.parse_xml |> Markup.signals |> Soup.from_signals in

	let () = keep_only_icons !icon_names soup in
	let () = attach_metadata !fa_version !fa_year soup in

	soup |> Soup.signals |> Markup.write_xml |> Markup.to_string |> print_endline
