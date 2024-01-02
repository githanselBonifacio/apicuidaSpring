package co.com.sura.web.moviles;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.moviles.MovilesUseCase;
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
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping("/moviles")
public class MovilesController {
    @Autowired
    private MovilesUseCase movilesUseCase;


    /**
     * consultar desplazamientos en turno
     * @return lista de desplazamientos
     * (Response<List<co.com.sura.moviles.entity.Desplazamiento.class>>)
     * @param fechaTurno fecha turno de la cita (LocalDate)
     * @param idRegional id regional del turno (String)
     * @param idHorarioTurno id horario del turno (Integer)
     * */
    @GetMapping(value = "/desplazamientoVisita")
    public Mono<Response<List<Desplazamiento>>> getDesplazamientoByTurnoRegional(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional ){
        return movilesUseCase.consultarDesplazamientoByTurnoRegional(fechaTurno,idHorarioTurno,idRegional)
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
