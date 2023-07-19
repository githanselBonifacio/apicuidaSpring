package co.com.sura.autoagendar;


import lombok.Data;
import org.javatuples.Pair;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

import static co.com.sura.autoagendar.Helper.*;

@Data
public class AutoAgendador {
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

    private  final Integer DOS = 2;
    private final Double UMBRAL = 0.5;
    public AutoAgendador(
            CitaGenetic origen,
            List<CitaGenetic> citas,
            Integer numeroMoviles,
            Integer numeroGeneraciones,
            Integer sizePoblacionInicial,
            Integer numeroPadresEnparejados,
            Double penalizacionHolgura) {

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
    }


    public void crearPoblacionInicial(){
       Poblacion poblacionInicial = new Poblacion();
       List<Individuo> individuos = new ArrayList<>();
       for (var i=0;i<this.sizePoblacionInicial;i++){
          Individuo individuo =  Individuo
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
                }else{
                    tiempoDesplazamiento = consultaTiempoDesplazamiento;

                }
                int tiempoTotalDesplazamiento = tiempoDesplazamiento +  citasIndividuo.get(c).getDuracion();

                int holguraAcumulada = (int) (citasIndividuo.get(c+1).getFechaInicioIso() -
                                        (citasIndividuo.get(c).getFechaInicioIso()+tiempoTotalDesplazamiento));

               /* System.out.println(tiempoDesplazamiento+" :: "+citasIndividuo.get(c).getDuracion()+" :: "+
                        citasIndividuo.get(c+1).getFechaInicioIso() +" :: "+citasIndividuo.get(c).getFechaInicioIso()
                +":: "+(citasIndividuo.get(c+1).getFechaInicioIso()-citasIndividuo.get(c).getFechaInicioIso())
                +":: "+holguraAcumulada);*/

                if (holguraAcumulada<0){
                    holgurasAcumuladas.add((int) ( -1*Math.abs(holguraAcumulada*(this.penalizacionHolgura))));
                }else{
                    holgurasAcumuladas.add(holguraAcumulada);
                }
            }
            puntajeAptitud.add(holgurasAcumuladas);

        }
     //   System.out.println("///////////////////////////////////////////////////////////////////////////");
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
                    encontrarHorguraNegativa(resultado.getPuntajeAptitudIndividuo(), (int)(Math.random()*DOS));

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
        return Collections.max(
                this.resultadoActual
                        .values(),Comparator.comparing(Resultado::getPuntajeAptitudGlobalIndividuo)
        );
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


    /*public static void main(String[] args) {

        List<CitaGenetic> citas = new ArrayList<>();
        citas.add(new CitaGenetic("1",10.998867,-74.827570,1800,900,
                LocalDateTime.of(2023, 7, 14, 8, 0).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("2",11.004831,-74.816292,3600,3600,
                LocalDateTime.of(2023, 7, 14, 10, 15).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("3",11.001397,-74.826874,900,27000,
                LocalDateTime.of(2023, 7, 14, 9, 30).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("4",10.993486,-74.820135,1200,900,
                LocalDateTime.of(2023, 7, 14, 11, 0).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("5",10.977166,-74.816718,4200,1800,
                LocalDateTime.of(2023, 7, 14, 8, 30).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("6",10.972345,-74.807658,4500,900,
                LocalDateTime.of(2023, 7, 14, 10, 45).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("7",10.970669,-74.810724,5200,1800,
                LocalDateTime.of(2023, 7, 14, 12, 30).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("8",10.968626,-74.804684,3600,900,
                LocalDateTime.of(2023, 7, 14, 8, 15).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("9",10.966256,-74.810394,1800,5500,
                LocalDateTime.of(2023, 7, 14, 10, 30).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("10",10.965350,-74.817936,7200,900,
                LocalDateTime.of(2023, 7, 14, 11, 40).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("11",10.989135,-74.808344,4800,2200,
                LocalDateTime.of(2023, 7, 14, 12, 0).toEpochSecond(ZoneOffset.UTC)));

        citas.add(new CitaGenetic("12",10.988777,-74.814695,2700,900,
                LocalDateTime.of(2023, 7, 14, 11, 50).toEpochSecond(ZoneOffset.UTC)));

        CitaGenetic origen = new CitaGenetic("0",11.001311083973969,-74.81237704232935,0,900,
                LocalDateTime.of(2023, 7, 14, 7, 0).toEpochSecond(ZoneOffset.UTC));

        AutoAgendador autoAgendador = new AutoAgendador(
                origen,
                citas,
                4,
                500,
                10,
                5,
                10e6
        );
        autoAgendador.run();
        Resultado mejorResultado = autoAgendador.mejorSolucion();

        System.out.println("////////////////////////////////////////////////////////////////////////////////");
        System.out.println(mejorResultado.getPuntajeAptitudGlobalIndividuo()+"-"+mejorResultado.getIndividuo());
        System.out.println(mejorResultado.getPuntajeAptitudIndividuo());
    }*/
}
