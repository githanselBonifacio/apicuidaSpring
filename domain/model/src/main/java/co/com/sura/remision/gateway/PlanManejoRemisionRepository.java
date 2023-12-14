package co.com.sura.remision.gateway;

import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.RemisionRequest;
import reactor.core.publisher.Mono;

import java.util.List;

public interface PlanManejoRemisionRepository {

    Mono<Void> registrarPlanManejo(RemisionRequest remisionRequest, List<CitaRequest> citasRequest
            ,Integer ultimoIdCita);
}
