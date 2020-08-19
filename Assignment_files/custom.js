function removeHash () { 
    var scrollV, scrollH, loc = window.location;
    if ("pushState" in history)
        history.pushState("", document.title, loc.pathname + loc.search);
    else {
        // Prevent scrolling by storing the page's current scroll offset
        scrollV = document.body.scrollTop;
        scrollH = document.body.scrollLeft;

        loc.hash = "";

        // Restore the scroll offset, should be flicker free
        document.body.scrollTop = scrollV;
        document.body.scrollLeft = scrollH;
    }
}

jQuery(document).ready(function($) {	
	// visibility controls handler
	$('.visibility-control a').each(function (i,e) {
		var $e = $(e);
		if (!$e.hasClass('active')) {
			$('.visibility-block.'+$e.attr('id')).hide();
		}
		$e.click(function() {
			$('.visibility-block').hide();
			$('.visibility-block.'+$e.attr('id')).show();
			$('.visibility-control a').removeClass('active');
			$e.addClass('active');
			return false;
		});
	});


	var last_in_line = function($listItems) {
		var offsets = [];

    		$listItems.each(function() {
        		offsets.push( $(this).offset().top );
    		});
		var result =$listItems[$listItems.length-1];

		for (var i=0;i<offsets.length-1;i++) {
			if (offsets[i+1] > offsets[i]) {
				result = $listItems[i];
				break;
			}
		}
		return result;
	}
	
	var compute_width_percentage = function(me, parent) {

	};

	//--- START TOOLBOX PAGE SCRIPT ---\\
	$(".folderContent").hide();
	
	//when a folder is clicked,
	//position the content folder after the clicked row
	//and toggle all folder / app icon that is not the one clicked.
	//and toggle the folder content panel
	$('.folder').css('cursor', 'pointer');
	$('.folder').click(function(event) {
		var openFolder = $(this).attr('id');
		var folderContent = $('.' + openFolder);
		folderContent.width($('.paper-sheet').outerWidth() - parseInt(folderContent.css("padding-left"), 10));
		var folderContentShown = $(folderContent).css("display") != "none";
		var clickedFolder = $(this);

		if (clickedFolder.hasClass('link')) { 
			// follow marked links
			clickedFolder.find('h2 a').get(0).click();
			// should be navigated now
			return true;
		}	
		
		//If there is no currently displayed folder details area...
		if ($(".folder-container  .active-folder").length == 0){
			var row = last_in_line(clickedFolder.add(clickedFolder.nextAll('.folder')));
			$(row).after(folderContent);
						
			$(this).addClass('active-folder', 200);
			$(folderContent).slideToggle("fast");
					
			$(".folder-container").find(".folder").not(clickedFolder).each(function() {
				if (!folderContentShown) {
					$(this).animate({ opacity: 0.20 }, "fast");
				}
				else {
					$(this).animate({ opacity: 1.00 }, "fast");
				}
			});
	
			if ($(row).is($('.toolbox-container .row').eq(1))) {
				$('.toolbox-container > .container:first-child').animate({ marginTop: -265 }, 200 );
			}
			if ($(row).is($('.toolbox-container .row').eq(0))) {
				$('.toolbox-container > .container:first-child').animate({ marginTop: -38 }, 200 );
			}
			
			
			//--Add the id to the URL but change it temporarily
			//--to keep it from scrolling to it
			hash = $(clickedFolder).attr('id');
			var node = $( '#' + hash );
			if ( node.length ) {
			  node.attr( 'id', '' );
			}
			document.location.hash = hash;
			if ( node.length ) {
			  node.attr( 'id', hash );
			}


		}
		
		//If there IS a currently displayed tool details area...
		else {
			if (folderContentShown) {
				//Active icon was clicked
				$(this).toggleClass("active-folder");
				
				$(folderContent).slideToggle("fast");
				$(".folder-container").find(".folder").not(clickedFolder).each(function() {
					if (!folderContentShown) {
						$(this).animate({ opacity: 0.20 }, "fast");
					}
					else {
						$(this).animate({ opacity: 1.00 }, "fast");
					}
				});
				
				//Reset the padding-top for the container
				$('.folder-container .container:first-child').animate({ marginTop: 0 }, 300 );
			}
			else {
				//Inactive icon was clicked
				$('.folderContent').slideUp("fast");
				$('.active-folder').removeClass('active-folder');
				$('.folder-container .folder').animate({ opacity: 1.00 }, "fast");
								
				//Reset the padding-top for the container
				$('.folder-container .container:first-child').animate({ marginTop: 0 }, 300 );
			}
			//window.location.hash="";
			//window.history.pushState("object or string","Title" , "/teaching/");
			removeHash();
		}
		
		event.preventDefault();
		return false;
	});
	// END TOOLBOX PAGE SCRIPT \\----------------
	
	
	// TOOLBOX - OPEN SECTION BY URL HASH \\
	if ($('.folder-container').length) {
		var clickedFolder = $(window.location.hash);
		var openFolder = $(clickedFolder).attr('id');
		var folderContent = $('.' + openFolder);
		var folderContentShown = $(folderContent).css("display") != "none";
		var row = last_in_line(clickedFolder.add(clickedFolder.nextAll('.folder')));
		$(row).after(folderContent);
					
		$('#' + openFolder).addClass('active-folder');
		$(folderContent).delay(200).slideDown();
				
		$(".folder-container").find(".folder").not(clickedFolder).each(function() {
			if (!folderContentShown) {
				$(this).css('opacity', '0.2');
			}
			else {
				$(this).css('opacity', '1.00');
			}
		});
		
	
		if ($(row).is($('.toolbox-container .row').eq(1))) {
			$('.toolbox-container > .container:first-child').animate({ marginTop: -265 }, 200 );
		}
		if ($(row).is($('.toolbox-container .row').eq(0))) {
			$('.toolbox-container > .container:first-child').animate({ marginTop: -38 }, 200 );
		}
		$('body').animate({scrollTop:0}, 200, 'linear');
		
		$('.what-we-do a').click(function(event) {
			var link = $(this).attr('href');
			window.location.href = link;
			window.location.reload();
			//return false;
		});
	}	
});
