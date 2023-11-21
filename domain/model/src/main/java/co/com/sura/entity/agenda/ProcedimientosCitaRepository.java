package co.com.sura.entity.agenda;

import co.com.sura.entity.remision.Canalizacion;
import co.com.sura.entity.remision.Curacion;
import co.com.sura.entity.remision.Fototerapia;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Secrecion;
import co.com.sura.entity.remision.Sondaje;
import co.com.sura.entity.remision.SoporteNutricional;
import co.com.sura.entity.remision.TomaMuestra;
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
