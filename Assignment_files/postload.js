jQuery.fn.immediateText = function() {
    return this.contents().not(this.children()).text();
};

var uf_ev = {};
var $dcalendar = jQuery('.col-md-6.dcalendar').first();
uf_ev.day = + $dcalendar.find('div.date').immediateText().trim();
if (uf_ev.day >=1 && uf_ev.day <= 31) {
	uf_ev.month = $dcalendar.find('.date .month').text().trim();
	var monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ];
	var today = new Date();

	uf_ev.pastDate = new Date(uf_ev.day + " " + uf_ev.month + " " + today.getFullYear() + " 17:00:00");

	if (today > uf_ev.pastDate) {
		// already past event
		$dcalendar.find('h2').text('Past Event');

	} else if (monthNames[today.getMonth()] == uf_ev.month && uf_ev.day == today.getUTCDate()) {
		// today!
		uf_ev.day_name = $dcalendar.find('.day').text()
		uf_ev.room = $dcalendar.data('room') || 'S1'
		uf_ev.time = $dcalendar.data('time') || '13:30'
		
		$dcalendar.find('.day').text('today')
		$dcalendar.find('.date').contents().first().each(function() {
			if (this.nodeType == 3) this.data = uf_ev.room
		});
		$dcalendar.find('.month').text(uf_ev.time)
		$dcalendar.find('h2').text('Today\'s Event')
	}
}
