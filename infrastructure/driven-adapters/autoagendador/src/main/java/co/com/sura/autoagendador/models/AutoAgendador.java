package co.com.sura.autoagendador.models;

import co.com.sura.mapbox.entity.GeoUbicacion;
import co.com.sura.mapbox.gateway.MapboxServiceRepository;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.javatuples.Pair;
import org.springframework.beans.factory.annotation.Autowired;

import java.security.SecureRandom;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;

import static co.com.sura.autoagendador.models.Helper.*;

@Data
@ToString
@NoArgsConstructor
public class AutoAgendador {
    private static final Integer DOS = 2;
    private static final Double UMBRAL = 0.5;
    private MapboxServiceRepository mapboxService;
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

    private final Random random= new SecureRandom();

    @Autowired
    public AutoAgendador(
            Integer numeroGeneraciones,
            Integer sizePoblacionInicial,
            Integer numeroPadresEnparejados,
            Double penalizacionHolgura,
            MapboxServiceRepository mapboxService) {

        this.numeroGeneraciones = numeroGeneraciones;
        this.sizePoblacionInicial = sizePoblacionInicial;
        this.numeroPadresEnparejados = numeroPadresEnparejados;
        this.penalizacionHolgura = penalizacionHolgura;

        this.desplazamientos = new ArrayList<>();
        this.resultadoActual = new HashMap<>();


        this.mapboxService =mapboxService;
    }

    public  AutoAgendador andOrigen(CitaGenetic origen){
        this.origen = origen;
        return this;
    }
    public  AutoAgendador withCitas(List<CitaGenetic> citas){
        this.citas = citas;
        return this;
    }
    public  AutoAgendador andNumeroMoviles(Integer numeroMoviles){
        this.numeroMoviles = numeroMoviles;
        return this;
    }

    /**
     * crea la población inicial de forma aleatoria en divisiones de acuerdo al número de móviles
     * */
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
    /**
     * Validar si se tiene información de desplazamientos entre dos citas,
     * si encuentra el desplazamiento devuelve el tiempo en segundos
     * @param idCita1 cita de partida (String)
     * @param idCita2 cita de destino (String)
     * @return duración de desplazamiento entre dos citas (Integer)
     * */
    private Integer validarDuracionViajeAlmacenado(String idCita1, String idCita2){
        for (DesplazamientoMap desplazamiento : this.desplazamientos) {
            if (desplazamiento.getPos1().equals(idCita1)
                    && desplazamiento.getPos2().equals(idCita2)) {
                return desplazamiento.getTiempo();
            }
        }
        return null;
    }
    /**
     * calcula el tiempo de desplazamiento en segundos entre dos ubicaciones geográficas
     * @param lat1 latitud punto inicial (Double)
     * @param lat2 latitud punto inicial (Double)
     * @param lon1 longitud punto final (Double)
     * @param lon2 latitud punto final (Double)
     * @return duración de desplazamiento entre dos ubicaciones (Integer)
     * */
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

        var tiempoViajeMono =mapboxService.calcularTiempoViaje(pos1,pos2);

        if (tiempoViajeMono != null) {
            return tiempoViajeMono.block();
        } else {
            return 0;
        }
    }
    /**
     calcular aptitud de individuo en el algoritmo genético, para validar la solución. El calculo se realiza mediente
     el calculo de holgura entre citas, las holguras positivas entre dos citas indica que se puede realizar la visita,
     las holguras negativas indican que dada las fechas y ubicaciones no es posible atender las citas
     de forma consecutiva.
     @param individuo solucion individual de una agrupación de citas (co.com.sura.autoagendador.models.Individuo.class)
     @return lista de puntajes de aptitud agrupados según la solución evaluada (List<List<Integer>>)
     */
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
                                     .fecha(Instant.ofEpochSecond(citasIndividuo.get(c).getFechaInicioIso())
                                             .atZone(ZoneId.of("America/Bogota")).toLocalDate())
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
    /**
     * calcula de forma global la solución de acuerdo a la lista de aptitudes individuales entre cada cita,
     * si el valor es positivo indica que la organización de citas cumple con holguras positivas para efectuarse.
     * si el valor es negativo quiere decir que al menos una cita esta mal ubicada y se sobrepone con la
     * cita anterior o siguiente
     * @param aptitudIndiviuo aptitudes individuales de casa cita en relacion con la cita sguiente (List<List<Integer>>)
     * @return aptitud global de la solución (double)
     */
    private static double calcularAptitudGlobalIndividuo(List<List<Integer>> aptitudIndiviuo){
        return aptitudIndiviuo.stream()
                .flatMap(List::stream)
                .mapToDouble(Integer::intValue)
                .sum();
    }
    /**
     * se redefine nueva poblacion tomando los mejores resultados, según el numero de padres emparejados
     * */
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
    /**
     * calcula la aptitud de la población actual.Los resultados son almacenados en el atributo resultadoActual
     * */
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
           .sorted(Map.Entry.comparingByValue(Comparator
                    .comparingDouble(Resultado::getPuntajeAptitudGlobalIndividuo)
                     .reversed()))
           .limit(this.numeroPadresEnparejados)
           .collect(
             Collectors.toMap(Map.Entry::getKey,Map.Entry::getValue,(oldValue,newValue)->oldValue, LinkedHashMap::new));

    }

    /**
     * encuentra las coordenadas de las citas que presentan holgura negativa, respecto a la cita siguiente o la anterior
     * @param puntajeIndividuo lista de holguras resultado del calculo de aptitud (List<List<Integer>>)
     * @param ic semilla aleatoria enrtre 0 y 1 para definir la coordenada de holgura negativa (int)
     * @return lista de coordenadas en la lista de aptitud donde se presenta la holgura negativa (List<Pair<Integer,Integer>>)
    * */
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
        return coordenadas;
    }

    /**
     * intercambio de citas en los diferentes móviles para generar nuevas combinaciones.
     * donde se encuentren holguras negativas se realiza un cambio a otro movil o grupo
     * return nueva poblacion (co.com.sura.autoagendador.models.Poblacion.class)
     * */
    private Poblacion mutarIndividuosPoblacion() {

        var nuevosIndividuos = new ArrayList<Individuo>();
        for (Resultado resultado: this.resultadoActual.values()) {

            var coor = encontrarHorguraNegativa(resultado.getPuntajeAptitudIndividuo(), random.nextInt(DOS));
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

    /**
     * calcula la mejor solucion según los datos actuales, devolvera la solucion con mayor puntaje.
     * se ejecuta al final de las iteraciones, si aun después de finalizar quedaron citas con holguras negativas,
     * en este metodo serán eliminadas
     * @return mejor resultado , individuo con mejor puntaje de aptitud (co.com.sura.autoagendador.models.Resultado.class)
     * */
    public Resultado mejorSolucion() {
        var mejorResultado = Collections.max(this.resultadoActual.values(),
                                                   Comparator.comparing(Resultado::getPuntajeAptitudGlobalIndividuo));
        var coordenadasHolguraNegativa = encontrarHorguraNegativa(mejorResultado.getPuntajeAptitudIndividuo(),0);
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
        nuevalistaCitasGen.forEach(v ->v.remove(0));
        mejorResultado.setIndividuo( new Individuo(nuevalistaCitasGen));
        return mejorResultado;
    }
    /**
     * reset data para realizar un nuevo calculo
     * */
    public void resetData(){
        this.citas = new ArrayList<>();
        this.origen = new CitaGenetic();
        this.numeroMoviles=0;
        this.resultadoActual = new HashMap<>();
        this.poblacionActual=new Poblacion();
        this.aptitudGlobalActual = 0.0;
    }
    /**
     * Elimina los desplazamientos de fechas anteriores a la fecha actual
     * @param fechaActual fecha a partir del cual se elimina registros (LocalDate)
     * */
    public void depurarFechasDesplazamientoCitaByFecha(LocalDate fechaActual){
        this.desplazamientos = this.desplazamientos.stream()
                .filter(d->!d.getFecha().isBefore(fechaActual))
                .collect(Collectors.toList());
    }
    /**
     * ejecutar algotitmo genetico para calcular una solución en cuanto a agrupación en la lista de citas
     * @return mejor resultado calculado (co.com.sura.autoagendador.models.Resultado.class)
     * */
    public Resultado run (){
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
        return this.mejorSolucion();
    }
}
