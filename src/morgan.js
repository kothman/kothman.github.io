let m = document;
m.qsa = m.querySelectorAll;
m.bid = m.getElementById;

function getColor(){ 
    return getRandomPastelColor();
}

function getRandomPastelColor() {
  // Random hue (0-360)
  const hue = Math.floor(Math.random() * 360);
  // Saturation: 20-40%
  const saturation = Math.floor(Math.random() * 30) + 40; // 30-70
  // Lightness: 70-90%
  const lightness = Math.floor(Math.random() * 20) + 40; // 20-70
 
  // Return HSL string (directly usable in CSS)
  return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}

function bloop_big_small(e) {
    e.classList.add("upsize-me");
    setTimeout(() => {
	e.classList.add("hide-me");
    }, 250);
    
    setTimeout(() => {
	e.remove();
    }, 1000);
}

function spark_joy() {
    let el = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    el.innerHTML = '<!-- Icon downloaded from https://icon-sets.iconify.design/mingcute/firework-line/ -->\
	<path d="M0 0h24v24H0z" fill="none" ></path>\
	    <path fill="none" stroke="currentColor" d="M21.502 11.542a.46.46 0 1 1 0 .917l-4.904-.41c-.061-.004-.061-.094 0-.099zm-19.004.917a.46.46 0 1 1 0-.917l4.904.408c.061.005.061.095 0 .1zM17.148 20a.46.46 0 1 1-.794.458l-2.098-4.451c-.026-.056.051-.1.086-.05zM6.852 4a.46.46 0 1 1 .794-.458l2.098 4.451c.026.056-.051.1-.086.05zm9.502-.458a.46.46 0 1 1 .794.459l-2.806 4.042c-.035.05-.112.006-.086-.05zM7.646 20.458A.46.46 0 1 1 6.852 20l2.806-4.043c.035-.05.112-.006.086.05zm3.896-15.96a.46.46 0 1 1 .917 0l-.41 4.904c-.004.061-.094.061-.099 0zm6.725 3.354a.46.46 0 1 1 .459.794l-4.451 2.098c-.056.026-.1-.051-.05-.086zm-12.993.794a.46.46 0 1 1 .459-.794l4.042 2.806c.05.035.006.112-.05.086zm7.185 10.856a.46.46 0 1 1-.918 0l.41-4.904c.004-.061.094-.061.099 0zm-6.726-3.354a.46.46 0 1 1-.459-.794l4.451-2.098c.056-.026.1.051.05.086zm12.993-.794a.46.46 0 1 1-.459.794l-4.042-2.806c-.05-.035-.006-.112.05-.086z" ></path>';
    el.setAttribute("viewBox", "0 0 24 24");
    el.setAttribute("width", "1em");
    el.setAttribute("height", "1em");
    el.setAttribute("alt", "fireworks !");
    let btn = m.bid("welcome-ok");
    let rect = btn.getBoundingClientRect();
    let n = Math.floor(Math.random() * 4);
    let point;
    switch (n) {
    case 0:
	point = {x: rect.x, y: rect.y};
	break;
    case 1:
        point = {x: rect.x, y: rect.y + rect.height};
	break;
    case 2:
        point = {x: rect.x + rect.width, y: rect.y};
	break;
    case 3:
        point = {x: rect.x + rect.width, y: rect.y + rect.height};
	break;
    default:
	console.log("error sparking joy?");
	break;
    }
    el.style.height = 2 + (Math.random() * 3) + "rem";
    el.style.width =  2 + (Math.random() * 3) + "rem";
    el.style.position = "absolute";
    el.style.top = point.y + "px";
    el.style.left = point.x + "px";
    el.style.opacity="0.0";
    el.style.color = getColor();
    console.log(el.style);
    document.body.appendChild(el);
    let compStyle = getComputedStyle(el);
    let randX = Math.floor(Math.random()*40) - 20 + 1;
    let randY = Math.floor(Math.random()*40) - 20 + 1;
    el.style.top = point.y - (parseInt(compStyle.height.split("px")[0]) / 2) + randY + "px";
    el.style.left = point.x - (parseInt(compStyle.width.split("px")[0]) / 2) + randX + "px";
    el.style.transform="scale(0.0)";
    el.style.opacity="1.0";
    setTimeout(() => {
	el.style.transition = "all ease-in-out .2s";
	el.style.transform="scale(1.0)";
	setTimeout(() => {
	    el.style.transition = "all ease-in-out .4s";
	    el.style.opacity = "0.0";
	    setTimeout(() => {
		el.remove();
	    }, 400);
	}, 400 );	
    }, 10);
}

function ok_welcome(e) {
    console.log("ok welcome");
    spark_joy();
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
