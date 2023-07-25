package co.com.sura.web.moviles;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.entity.moviles.Desplazamiento;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;

import java.time.LocalDate;

@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/moviles")
public class MovilesController {
    @Autowired
    private AgendaUseCase agendaUseCase;

    @GetMapping(value = "/desplazamientoVisita")
    public Flux<Desplazamiento> getDesplazamientoByIdCitaPartida(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idCiudad ){
        return agendaUseCase.consultarDesplazamientoByIdCitaPartida(fechaTurno,idHorarioTurno,idCiudad);
    }
}
