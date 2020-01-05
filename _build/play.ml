(** 
   [static_screen logo dest color] is a screen displaying
   the text logo at [logo] in color [color], and
   calls [dest] when the user clicks on it
*)
let static_screen logo dest color =
  let scr : Screen.screen = {
    background = Graphics.black; 
    shapes = [ Screen.Text (
        {x = 0.5; y = 0.5; width = 0.5; height = 0.5;}, 
        {color = color; width = 4}, logo |> Utils.read_lines) ]
  }
  in Screen.draw_screen scr;
  let rec loop () =
    let stat = Graphics.wait_next_event 
        [Graphics.Button_down; Graphics.Key_pressed] in
    Input.click_or_quit stat loop (fun _ -> dest ()) in loop ()


(** [init ()] opens the connect 4 window *)
let rec init () =
  Graphics.open_graph " 800x600";
  Graphics.set_window_title "Connect 4";
  at_exit (Graphics.close_graph);
  title_screen ()


and title_screen () =
  static_screen "./logos/connect4_logo.txt" main_menu Graphics.white
and yellow_win () =
  static_screen "./logos/yellow_win.txt" main_menu 
    (Settings.get_color YellowSelection)
and red_win () =
  static_screen "./logos/red_win.txt" main_menu 
    (Settings.get_color RedSelection)
and tie () =
  static_screen "./logos/draw.txt" main_menu Graphics.white

and main_menu () =
  let human : Window.region = 
    {x = 0.5; y = 0.53; width = 0.1; height = 0.1;} in
  let human_text = ["- HUMAN -"] in
  let computer : Window.region = 
    {x = 0.5; y = 0.47; width = 0.1; height = 0.1;} in
  let computer_text = ["- COMPUTER -"] in
  let settings : Window.region =
    {x = 0.5; y = 0.25; width = 0.1; height = 0.1;} in
  let settings_text = ["- SETTINGS -"] in
  let scr : Screen.screen = 
    {
      background = Graphics.black; 
      shapes = [
        Screen.Text ({x = 0.5; y = 0.8; width = 0.1; height = 0.1;}, 
                     {color = Graphics.white; width = 4}, 
                     ["*** CHOOSE AN OPPONENT ***"]);
        Screen.Text (human, {color = Graphics.white; width = 4}, human_text);

        Screen.Text (computer, {color = Graphics.white; width = 4}, 
                     computer_text);

        Screen.Text (settings, {color = Graphics.white; width = 4},
                     settings_text);
      ]
    }
  in
  Screen.draw_screen scr;

  let cc : Input.click_collection = [
    {region = Draw.text_region human human_text; 
     callback = (fun () -> game (State.make_start_state 6 7) 
                    Human.Human.move Human.Human.move)};
    {region = Draw.text_region computer computer_text; 
     callback = (fun () -> difficulty ())};
    {region = Draw.text_region settings settings_text;
     callback = (fun () -> theme_select ())}
  ]
  in
  let rec loop () =
    let stat = Graphics.wait_next_event 
        [Graphics.Button_down; Graphics.Key_pressed] in
    Input.click_or_quit stat loop
      (fun stat -> Input.run_callbacks cc loop
          (Window.relativize (stat.mouse_x, stat.mouse_y))) in loop ()

and difficulty () =
  let easy : Window.region = 
    {x = 0.5; y = 0.55; width = 0.1; height = 0.1;} in
  let easy_text = ["- EASY -"] in
  let medium : Window.region = 
    {x = 0.5; y = 0.5; width = 0.1; height = 0.1;} in
  let medium_text = ["- MEDIUM -"] in
  let hard : Window.region = 
    {x = 0.5; y = 0.45; width = 0.1; height = 0.1;} in
  let hard_text = ["- HARD -"] in
  let scr : Screen.screen = {
    background = Graphics.black; 
    shapes = [
      Screen.Text 
        ({x = 0.5; y = 0.8; width = 0.1; height = 0.1;}, 
         {color = Graphics.white; width = 4}, ["CHOOSE A DIFFICULTY"]);

      Screen.Text (easy, {color = Graphics.white; width = 4}, easy_text);

      Screen.Text (medium, {color = Graphics.white; width = 4}, medium_text);

      Screen.Text (hard, {color = Graphics.white; width = 4}, hard_text);
    ]
  } in Screen.draw_screen scr;

  let cc : Input.click_collection = [
    {region = Draw.text_region easy easy_text; 
     callback = (fun () -> game (State.make_start_state 6 7) 
                    Human.Human.move Computer.Easy.move)};

    {region = Draw.text_region medium medium_text; 
     callback = (fun () -> game (State.make_start_state 6 7) 
                    Human.Human.move Computer.Medium.move)};

    {region = Draw.text_region hard hard_text; 
     callback = (fun () -> game (State.make_start_state 6 7) 
                    Human.Human.move Computer.Hard.move)};
  ] in
  let rec loop () =
    let stat = Graphics.wait_next_event 
        [Graphics.Button_down; Graphics.Key_pressed] in
    Input.click_or_quit stat loop
      (fun stat -> Input.run_callbacks cc loop
          (Window.relativize (stat.mouse_x, stat.mouse_y))) in loop ()

and theme_select () =
  let pen : Draw.pen = {color = Graphics.white; width = 1} in

  let style_region : Window.region = 
    {x = 0.5; y = 0.5; width = 0.1; height = 0.1} in
  let style_de_text = ["<<#"] in
  let style_ad_text = ["#>>"] in
  let style_de_y : Window.region = {style_region with x = 0.1;} in
  let style_ad_y : Window.region = {style_region with x = 0.4;} in
  let style_de_r : Window.region = {style_region with x = 0.6;} in
  let style_ad_r : Window.region = {style_region with x = 0.9;} in

  let color_region : Window.region = 
    {x = 0.5; y = 0.6; width = 0.1; height = 0.1} in
  let color_de_text = ["<<%"] in
  let color_ad_text = ["%>>"] in
  let color_de_y : Window.region = {color_region with x = 0.1;} in
  let color_ad_y : Window.region = {color_region with x = 0.4;} in
  let color_de_r : Window.region = {color_region with x = 0.6;} in
  let color_ad_r : Window.region = {color_region with x = 0.9;} in

  let bg_region : Window.region = 
    {x = 0.5; y = 0.2; width = 0.15; height = 0.15;} in
  let bg_de : Window.region = {bg_region with x = 0.35} in
  let bg_ad : Window.region = {bg_region with x = 0.65} in

  let back : Window.region =
    {x = 0.5; y = 0.05; width = 0.15; height = 0.15;} in
  let back_text = ["< BACK >"] in
  let scr : Screen.screen = 
    {
      background = Graphics.black; 
      shapes = [
        Screen.Text 
          ({x = 0.25; y = 0.8; width = 0.1; height = 0.1;}, 
           {color = Graphics.yellow; width = 4}, ["YELLOW"]);

        Settings.get_shape Settings.YellowPiece 
          {x = 0.25; y = 0.5; width = 0.3; height = 0.3;};

        Screen.Text ({x = 0.25; y = 0.3; width = 0.1; height = 0.1;},
                     pen, [Settings.get_desc YellowPiece]);

        Screen.Text (style_de_y, pen, style_de_text);
        Screen.Text (style_ad_y, pen, style_ad_text);

        Screen.Text (color_de_y, pen, color_de_text);
        Screen.Text (color_ad_y, pen, color_ad_text);

        Screen.Text ({x = 0.75; y = 0.8; width = 0.1; height = 0.1;}, 
                     {color = Graphics.red; width = 4}, ["RED"]);

        Settings.get_shape Settings.RedPiece 
          {x = 0.75; y = 0.5; width = 0.3; height = 0.3;};

        Screen.Text ({x = 0.75; y = 0.3; width = 0.1; height = 0.1;},
                     pen, [Settings.get_desc RedPiece]);

        Screen.Text (style_de_r, pen, style_de_text);
        Screen.Text (style_ad_r, pen, style_ad_text);

        Screen.Text (color_de_r, pen, color_de_text);
        Screen.Text (color_ad_r, pen, color_ad_text);

        Screen.Rect 
          ({x = 0.5; y = 0.2; width = 0.2; height = 0.2;}, 
           {color = Settings.get_color Settings.Background; width = 1});

        Screen.RectOutline
          ({x = 0.5; y = 0.2; width = 0.15; height = 0.15;}, 
           {color = Settings.get_alt_color Settings.Background; width = 8});

        Screen.Text (bg_de, pen, color_de_text);
        Screen.Text (bg_ad, pen, color_ad_text);

        Screen.Text (back, pen, back_text);
      ]
    }
  in
  Screen.draw_screen scr;

  let cc_setting_change r txt cmd tar = 
    ({region = Draw.text_region r txt;
      callback = (fun () -> cmd tar; theme_select ())}:Input.click_region)

  in
  let cc : Input.click_collection = [
    (* style changing callbacks *)
    cc_setting_change style_de_y style_de_text 
      Settings.devance_style YellowPiece;
    cc_setting_change style_ad_y style_ad_text 
      Settings.advance_style YellowPiece;
    cc_setting_change style_de_r style_de_text 
      Settings.devance_style RedPiece;
    cc_setting_change style_ad_r style_ad_text 
      Settings.advance_style RedPiece;

    (* color changing callbacks *)
    cc_setting_change color_ad_y color_ad_text 
      Settings.advance_color YellowSelection;
    cc_setting_change color_de_y color_de_text 
      Settings.devance_color YellowSelection;
    cc_setting_change color_ad_r color_ad_text 
      Settings.advance_color RedSelection;
    cc_setting_change color_de_r color_de_text 
      Settings.devance_color RedSelection;
    cc_setting_change bg_ad color_ad_text 
      Settings.advance_color Background;
    cc_setting_change bg_de color_de_text 
      Settings.devance_color Background;

    {
      region = Draw.text_region back back_text;
      callback = (fun () -> main_menu ())
    };
  ] in
  let rec loop () =
    let stat = Graphics.wait_next_event 
        [Graphics.Button_down; Graphics.Key_pressed] in
    Input.click_or_quit stat loop
      (fun stat -> 
         Input.run_callbacks cc loop 
           (Window.relativize (stat.mouse_x, stat.mouse_y))) in loop ()

and game board curr next =
  match State.get_turn board with
  | State.Tie -> tie ()
  | State.YellowWins -> yellow_win ()
  | State.RedWins -> red_win ()
  | State.YellowTurn | State.RedTurn -> 
    begin
      Screen.draw_screen {
        background = Settings.get_color Background;
        shapes = [(Board_draw.board_shapes (7,6) board)]
      };
      try game (curr board) next curr with | Human.Quit -> ()
    end

let () = init ()
