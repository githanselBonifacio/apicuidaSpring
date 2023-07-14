package co.com.sura.entity.remision;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.RemisionRequest;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Date;
import java.util.List;

public interface RemisionCrudRepository {
    Mono<Void> crearRemisionCita (RemisionRequest remisionRequest, List<CitaRequest> citas);
}
