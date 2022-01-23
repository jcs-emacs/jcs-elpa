/**
 * $File: main.js $
 * $Date: 2022-01-22 23:53:52 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const base_url = "https://github.com/jcs-emacs/jcs-elpa/blob/master/";
const base_url_raw = "https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/";
const archive_url = base_url_raw + "master/docs/archive.json";

$(document).ready(function(){
  var packageList = $('#package-list');

  $.getJSON(archive_url, function(archive){
    for (let index = 0; index < archive.length; ++index) {
      let desc = archive[index];
      packageList.append(
        '<tr>' +
          '<td>' + desc.name + '</td>' +
          '<td>' + desc.summary + '</td>' +
          '<td>' + desc.version + '⭳</td>' +
          '<td><a href="' + base_url + 'recipes/' + desc.name + '">🍴</a></td>' +
          '<td><a href="' + desc.url + '">' + desc.source + '</a></td>' +
          '<td><img src="' + base_url_raw + 'master/badges/' + desc.name + '.svg"/></td>' +
          '</tr>');
    }
  }).fail(function(){
    console.log("An error has occurred.");
  });
});
