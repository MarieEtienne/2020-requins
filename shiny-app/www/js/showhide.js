(function($) {
var $clickme = $('.clickme'),
  $box = $('.box');

$box.hide();

$clickme.click(function(e) {
  $(this).text(($(this).text() === 'Hide' ? 'Show' : 'Hide')).next('.box').slideToggle();
  e.preventDefault();
  
});

})(jQuery);