var map = L.map('map').setView([39.9526, -75.1652], 12);
var layer = L.esri.Vector.basemap('ModernAntique');
layer.addTo(map);

var phillyData = L.esri.featureLayer({
  url: 'https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/Farmers_Markets/FeatureServer/0'
}).addTo(map);

phillyData.bindPopup(function (layer) {
    return layer.feature.properties.NAME;
});
