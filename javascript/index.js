/**
 * Main JS file for Casper behaviours
 */

/*globals jQuery, document */
(function ($) {
    "use strict";

    $(document).ready(function(){

        $(".post-content").fitVids();

        var page_url = document.URL
        $(".icon-twitter").attr("href", "https://twitter.com/share?url=" + page_url);
        $(".icon-facebook").attr("href", "https://www.facebook.com/sharer/sharer.php?u=" + page_url);
        $(".icon-google-plus").attr("href", "https://plus.google.com/share?url=" + page_url);
    });

}(jQuery));
