package co.com.sura.remision.gateway;

import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface HistorialRemisionRepository {

    Flux<RegistroHistorialRemision> consultarHistoricoRemision(String idRemision);
    Mono<RegistroHistorialRemision> consultarDatosRemision(String idRemision);

}
