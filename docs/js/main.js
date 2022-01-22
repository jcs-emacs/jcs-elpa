/**
 * $File: main.js $
 * $Date: 2022-01-22 23:53:52 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const archive_url = "https://raw.githubusercontent.com/jcs-emacs/elpa/master/docs/archive.json";

$(document).ready(function(){
  $.getJSON(archive_url, function(data){
    console.log(data);
  }).fail(function(){
    console.log("An error has occurred.");
  });
});
