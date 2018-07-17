$('.box-header').click(function() {
    $(this).toggleBox();
});


$('#DT_configSelect').on('click.dt', 'tbody tr', function (e, dt, type, cell, originalEvent) {
    $('#debugOut').html("noSel");
    $(this).addClass('selected');
    $('#debugDetails').html($(this).attr('class'));
    if ($(this).hasClass('selected')) {
        $('#debugOut').html("yesSel");
    }
});

$('#DT_colorsOrder').on('click.dt', 'tbody tr', function (e, dt, type, cell, originalEvent) {
    $('#debugOut').html("noSel");
    $(this).addClass('selected');
    $('#debugDetails').html($(this).attr('class'));
    if ($(this).hasClass('selected')) {
        $('#debugOut').html("yesSel");
    }
});

