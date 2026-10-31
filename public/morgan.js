let m = document;
m.qsa = m.querySelectorAll;
m.bid = m.getElementById;

function bloop_big_small(e) {
    e.classList.add("upsize-me");
    setTimeout(() => {
	e.classList.add("hide-me");
    }, 250);
    
    setTimeout(() => {
	e.remove();
    }, 1000);
}

function bloop_spin (e) {
    e.classList.add("spin-me");
    setTimeout(() => {
	e.remove();
    }, 2000);
}

function bloop_skadoosh_left_right(e) {
    e.classList.add("skadoosh-left-a-little");
    setTimeout(() => {
	e.classList.add("skadoosh-right");
    }, 500);    
}

function ok_welcome(e) {
    console.log("ok welcome");
    bloop_spin(m.bid("welcome"));
    bloop_skadoosh_left_right(m.bid("welcome-container"));
}

function close_welcome(e) {
    console.log("goodbye :c")
    let welcome_e = m.bid("welcome");
    bloop_big_small(welcome_e);
}


document.addEventListener("DOMContentLoaded", () => {
    console.log("DOMContentLoaded");
    m.bid("welcome-ok").addEventListener("click", ok_welcome);
    m.bid("close-ok").addEventListener("click", close_welcome);
    console.log("added event listeners");
});
