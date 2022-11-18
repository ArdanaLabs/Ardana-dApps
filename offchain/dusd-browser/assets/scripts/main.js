const lightmodeKey = "dusd-browser-lightmode";
const currentLightmode = localStorage.getItem(lightmodeKey);
const lightmodeCss = document.getElementById("lightmode-css");
const lightmodeToggleBtn = document.getElementById("lightmode-toggle-btn");

if (typeof currentLightmode == "undefined" || currentLightmode == "night") {
  lightmodeCss.href = "./assets/styles/night.css";
} else {
  lightmodeCss.href = "./assets/styles/light.css";
}

lightmodeToggleBtn.addEventListener("click", function (e) {
  const currentLightmode = localStorage.getItem(lightmodeKey);

  if (typeof currentLightmode == "undefined" || currentLightmode == "night") {
    lightmodeCss.href = "./assets/styles/light.css";
    localStorage.setItem(lightmodeKey, "light");
    lightmodeToggleBtn.blur();
  } else {
    lightmodeCss.href = "./assets/styles/night.css";
    localStorage.setItem(lightmodeKey, "night");
    lightmodeToggleBtn.blur();
  }
});
