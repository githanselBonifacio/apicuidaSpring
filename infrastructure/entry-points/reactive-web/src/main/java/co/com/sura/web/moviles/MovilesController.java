package co.com.sura.web.moviles;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.genericos.Response;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/moviles")
public class MovilesController {
    @Autowired
    private AgendaUseCase agendaUseCase;

    @GetMapping(value = "/desplazamientoVisita")
    public Mono<Response<List<Desplazamiento>>> getDesplazamientoByIdCitaPartida(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional ){
        return agendaUseCase.consultarDesplazamientoByIdCitaPartida(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(desplazamientos -> ResponseFactory.createStatus(
                        desplazamientos,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
}
