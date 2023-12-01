package co.com.sura.entity.agenda;

import co.com.sura.entity.remision.procedimientos.Canalizacion;
import co.com.sura.entity.remision.procedimientos.Curacion;
import co.com.sura.entity.remision.procedimientos.Fototerapia;
import co.com.sura.entity.remision.procedimientos.Procedimientos;
import co.com.sura.entity.remision.procedimientos.Secrecion;
import co.com.sura.entity.remision.procedimientos.Sondaje;
import co.com.sura.entity.remision.procedimientos.SoporteNutricional;
import co.com.sura.entity.remision.procedimientos.TomaMuestra;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ProcedimientosCitaRepository {

    //procedimientos
    Mono<Procedimientos> consultarProcedimientosByIdCita(String idCita);
    //curaciones
    Flux<Curacion> consultarCuracionesByCitas(String idCita);

    //Canalizaciones
    Flux<Canalizacion>      consultarCanalizacionesByCitas(String idCita);

    //Fototerapias
    Flux<Fototerapia>       consultarFototerapiasByCitas(String idCita);

    //Secreciones
    Flux<Secrecion>         consultarSecrecionesByCitas(String idCita);

    //sondaje
    Flux<Sondaje>           consultarSondajesByCitas(String idCita);

    //toma de muestras
    Flux<TomaMuestra>        consultarTomaMuestrasByCitas(String idCita);

    //soporte nutricionales
    Flux<SoporteNutricional> consultarSoporteNutricionalesByCitas(String idCita);
}
