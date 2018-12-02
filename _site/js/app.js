(function () {
    document.addEventListener("DOMContentLoaded", function() {
	init();
    });

    function init() {
	addEventListeners();
    }

    function addEventListeners() {
	document.querySelector("#toggle").addEventListener("click", toggleNav);
	var emailButton = document.querySelector(".nav-item a[href^='mailto:']");
	emailButton.addEventListener("mouseenter", toggleEnvelope);
	emailButton.addEventListener("mouseleave", toggleEnvelope);
    }

    function toggleNav() {
	var nav = document.querySelector(".nav-menu");
	nav.style.display == "" ?
	    nav.style.display = "block" :
	    nav.style.display = "" ;
    }

    function toggleEnvelope(e) {
	var i = e.currentTarget.querySelector("i");
	if (i.classList.contains("fa-envelope")) {
	    i.classList.remove("fa-envelope");
	    i.classList.add("fa-envelope-open");
	} else {
	    i.classList.add("fa-envelope");
	    i.classList.remove("fa-envelope-open");
	}
    }

})();

function setActive(selector) {
    var navItems = document.querySelectorAll('.nav-item');
    for(var i = 0; i < navItems.length; i++) {
	if(navItems[i].classList.contains('is-active'))
	    navItems[i].classList.remove('is-active')
    }
    document.querySelector(selector).classList.add('is-active');
}
