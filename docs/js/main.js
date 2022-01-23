/**
 * $File: main.js $
 * $Date: 2022-01-22 23:53:52 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const base_url = "https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/";
const archive_url = base_url + "master/docs/archive.json";

$(document).ready(function(){
  var packageList = $('#package-list');

  $.getJSON(archive_url, function(archive){
    for (let index = 0; index < archive.length; ++index) {
      let desc = archive[index];
      packageList.append(
        '<tr>' +
          '<td>' + desc.name + '</td>' +
          '<td>' + desc.summary + '</td>' +
          '<td>' + desc.version + '</td>' +
          '<td>' + 'github' + '</td>' +
          '<td><img src="' + base_url + 'master/badges/' + desc.name + '.svg"/></td>' +
          '</tr>');
    }
  }).fail(function(){
    console.log("An error has occurred.");
  });
});
