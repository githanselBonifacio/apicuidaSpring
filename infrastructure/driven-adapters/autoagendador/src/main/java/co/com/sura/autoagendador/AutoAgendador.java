package co.com.sura.autoagendador;

import co.com.sura.services.mapbox.GeoUbicacion;
import co.com.sura.services.mapbox.MapboxService;
import lombok.Data;
import org.javatuples.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


import java.util.*;
import java.util.stream.Collectors;

import static co.com.sura.autoagendador.Helper.*;

@Data
public class AutoAgendador {

    private MapboxService mapboxService;
    private static final Integer DOS = 2;
    private static final Double UMBRAL = 0.5;
    private Helper helper;
    private CitaGenetic origen;
    private List<CitaGenetic> citas;
    private Integer numeroMoviles;
    private Integer numeroGeneraciones;
    private Integer sizePoblacionInicial;
    private Integer numeroPadresEnparejados;
    private double penalizacionHolgura;
    private Runnable calcularDesplazamiento;

    private Map<String,Resultado> resultadoActual;
    private List<DesplazamientoMap> desplazamientos;
    private Poblacion poblacionActual;
    private Double aptitudGlobalActual;

    private final Random random;

    @Autowired
    public AutoAgendador(
            CitaGenetic origen,
            List<CitaGenetic> citas,
            Integer numeroMoviles,
            Integer numeroGeneraciones,
            Integer sizePoblacionInicial,
            Integer numeroPadresEnparejados,
            Double penalizacionHolgura,
            MapboxService mapboxService) {

        this.origen = origen;
        this.citas = new ArrayList<>(citas);
        this.numeroMoviles = numeroMoviles;
        this.numeroGeneraciones = numeroGeneraciones;
        this.sizePoblacionInicial = sizePoblacionInicial;
        this.numeroPadresEnparejados = numeroPadresEnparejados;
        this.penalizacionHolgura = penalizacionHolgura;

        this.desplazamientos = new ArrayList<>();
        this.resultadoActual = new HashMap<>();

        this.random = new Random();
        this.mapboxService =mapboxService;
    }


    public void crearPoblacionInicial(){
       var poblacionInicial = new Poblacion();
       List<Individuo> individuos = new ArrayList<>();
       for (var i=0;i<this.sizePoblacionInicial;i++){
          var individuo =  Individuo
                   .builder()
                   .citaGen(
                       new ArrayList<>(
                       CitaGenetic.dividirListaCitas(
                           this.citas,
                            this.numeroMoviles,
                            this.origen)
                           )
                   )
                   .build();
           individuos.add(individuo);
       }
       poblacionInicial.setIndividuos(individuos);
       this.poblacionActual = poblacionInicial;
    }
    private Integer validarDuracionViajeAlmacenado(String idCita1, String idCita2){
        for (DesplazamientoMap desplazamiento : this.desplazamientos) {
            if (desplazamiento.getPos1().equals(idCita1)
                    && desplazamiento.getPos2().equals(idCita2)) {
                return desplazamiento.getTiempo();
            }
        }
        return null;
    }
    public  Integer calcularTiempoDesplazamiento(Double lat1,Double lon1,Double lat2, Double lon2 ){

        var pos1 = GeoUbicacion
                .builder()
                .latitud(lat1)
                .longitud(lon1)
                .build();

        var pos2 =GeoUbicacion
                .builder()
                .latitud(lat2)
                .longitud(lon2)
                .build();

        var tiempoViajeMono =mapboxService.calcularTiempoViajeMapboxSDK(pos1,pos2);
        if (tiempoViajeMono != null) {
            return tiempoViajeMono.block();
        } else {
            return 0;
        }
    }
    private List<List<Integer>> calcularAptitudIndividuo(Individuo individuo){

        List <List<Integer>> puntajeAptitud= new ArrayList<>();


        for (List<CitaGenetic> citasIndividuo: individuo.getCitaGen()){
            List<Integer> holgurasAcumuladas = new ArrayList<>();
            for (var c = 0;c<citasIndividuo.size();c++){
                if (c == citasIndividuo.size()-1){
                    break;
                }
                Integer tiempoDesplazamiento;
                Integer consultaTiempoDesplazamiento = validarDuracionViajeAlmacenado(
                        citasIndividuo.get(c).getIdCita(),
                        citasIndividuo.get(c+1).getIdCita()
                );
                if (consultaTiempoDesplazamiento == null){
                     tiempoDesplazamiento = calcularTiempoDesplazamiento(
                            citasIndividuo.get(c).getLatitud(),
                            citasIndividuo.get(c).getLongitud(),
                            citasIndividuo.get(c+1).getLatitud(),
                            citasIndividuo.get(c+1).getLongitud()

                    );
                     this.desplazamientos.add(
                             new DesplazamientoMap()
                                     .toBuilder()
                                     .pos1(citasIndividuo.get(c).getIdCita())
                                     .pos2(citasIndividuo.get(c+1).getIdCita())
                                     .tiempo(tiempoDesplazamiento)
                                     .build());
                }else{
                    tiempoDesplazamiento = consultaTiempoDesplazamiento;

                }
                int tiempoTotalDesplazamiento = tiempoDesplazamiento +  citasIndividuo.get(c).getDuracion();

                int holguraAcumulada = (int) (citasIndividuo.get(c+1).getFechaInicioIso() -
                                        (citasIndividuo.get(c).getFechaInicioIso()+tiempoTotalDesplazamiento));


                if (holguraAcumulada<0){
                    holgurasAcumuladas.add((int) ( -1*Math.abs(holguraAcumulada*(this.penalizacionHolgura))));
                }else{
                    holgurasAcumuladas.add(holguraAcumulada);
                }
            }
            puntajeAptitud.add(holgurasAcumuladas);

        }
        return puntajeAptitud;
    }
    private static double calcularAptitudGlobalIndividuo(List<List<Integer>> aptitudIndiviuo){
        return aptitudIndiviuo.stream()
                .flatMap(List::stream)
                .mapToDouble(Integer::intValue)
                .sum();
    }
    private  Poblacion seleccionarMejoresPadres(){
       return new Poblacion(
               this.resultadoActual
                   .entrySet()
                   .stream()
                   .limit(this.numeroPadresEnparejados)
                   .map(Map.Entry::getValue)
                   .map(Resultado::getIndividuo)
                   .collect(Collectors.toList())
       );
    }
    private void calcularAptitudPoblacion(){
        for (var i=0;i<this.poblacionActual.getIndividuos().size();i++){
            var puntajeAptitudIndividuo = this.calcularAptitudIndividuo(poblacionActual.getIndividuos().get(i));
            var puntajeGlobalAptitudIndividuo = calcularAptitudGlobalIndividuo(puntajeAptitudIndividuo);
            this.resultadoActual.put(
                    UUID.randomUUID().toString(),
                    new Resultado()
                            .toBuilder()
                            .puntajeAptitudIndividuo(puntajeAptitudIndividuo)
                            .individuo(poblacionActual.getIndividuos().get(i))
                            .puntajeAptitudGlobalIndividuo(puntajeGlobalAptitudIndividuo)
                            .build());
        }
        this.resultadoActual = this.resultadoActual
                .entrySet()
                .stream()
                .sorted(
                        Map.Entry
                                .comparingByValue(
                                        Comparator.comparingDouble(Resultado::getPuntajeAptitudGlobalIndividuo
                                                )
                                .reversed())
                )
                .limit(this.numeroPadresEnparejados)
                .collect(
                        Collectors.toMap(
                                Map.Entry::getKey,Map.Entry::getValue,(oldValue,newValue)->oldValue, LinkedHashMap::new)
                );

    }

    private static  List<Pair<Integer,Integer>> encontrarHorguraNegativa(List<List<Integer>> puntajeIndividuo, int ic){
        var coordenadas = new ArrayList<Pair<Integer,Integer>>();
        for (var i=0;i<puntajeIndividuo.size();i++){
            for (var j=0;j<puntajeIndividuo.get(i).size();j++){
                if(puntajeIndividuo.get(i).get(j)<0){
                    var c = (j+ic<puntajeIndividuo.get(i).size()) ? (j+ic) : j;
                    coordenadas.add(new Pair<>(i,c));
                }
            }
        }
        return coordenadas
                .stream()
                .filter(tupla -> tupla.getValue1() !=0 )
                .collect(Collectors.toList());
    }

    private Poblacion mutarIndividuosPoblacion() {

        var nuevosIndividuos = new ArrayList<Individuo>();

        for (Resultado resultado: this.resultadoActual.values()) {

            var coor =
                    encontrarHorguraNegativa(resultado.getPuntajeAptitudIndividuo(), random.nextInt(DOS));

            if (coor.isEmpty()){
                return new Poblacion();
            }
            var pos1 = coor.get(random.nextInt(coor.size()));
            var pos2 = new Pair<>(maxSumaLista(resultado.getPuntajeAptitudIndividuo()),1);
            Individuo nuevoIndividuo;

            var individuoMutar = resultado.getIndividuo().clone();
            if (random.nextDouble() > UMBRAL) {
                nuevoIndividuo = intercambiarElementos(individuoMutar.getCitaGen(), pos1, pos2);
            } else {
                nuevoIndividuo = cambiarElementos(individuoMutar.getCitaGen(), pos1, pos2);
            }

            nuevosIndividuos.add(nuevoIndividuo);
        }
        return new Poblacion(
                nuevosIndividuos
                        .stream()
                        .peek(Individuo::sortCitaGen)
                        .collect(Collectors.toList())
        );
    }

    public Resultado mejorSolucion() {
        var mejorResultado = Collections.max(
                this.resultadoActual
                        .values(),Comparator.comparing(Resultado::getPuntajeAptitudGlobalIndividuo)
        );
        List<Pair<Integer,Integer>> coordenadasHolguraNegativa = encontrarHorguraNegativa(
                mejorResultado.getPuntajeAptitudIndividuo(),1
        );
        var individuo = mejorResultado.getIndividuo();
        List<List<CitaGenetic>> nuevalistaCitasGen = new ArrayList<>();

        for(var i = 0;i<individuo.getCitaGen().size();i++){
            List<CitaGenetic> itemIndividuo = new ArrayList<>();
            for(var  j = 0; j<individuo.getCitaGen().get(i).size();j++){
                if(!coordenadasHolguraNegativa.contains(new Pair<>(i,j))){
                    itemIndividuo.add(individuo.getCitaGen().get(i).get(j));
                }

            }
            nuevalistaCitasGen.add(itemIndividuo);
        }

        mejorResultado.setIndividuo( new Individuo(nuevalistaCitasGen));
        return mejorResultado;
    }
    public void run (){
        this.crearPoblacionInicial();
        for(var g = 0;g<this.numeroGeneraciones;g++){
            this.calcularAptitudPoblacion();
            this.poblacionActual = this.seleccionarMejoresPadres();
            var siguienteGeneracion = this.mutarIndividuosPoblacion();
            if(siguienteGeneracion.getIndividuos().isEmpty()){
                break;
            }else{
                this.poblacionActual = siguienteGeneracion.clone();
            }
        }
    }
}
